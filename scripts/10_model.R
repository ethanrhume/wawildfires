# =============================================================================
# Script:  10_model.R
# Author:  Ethan Hume
# Date:    2026-06-16
# Inputs:  1. Analytic dataset (data/analytic_2015_2018.csv).
# Purpose: Uses the LightGBM framework to predict individual acute care 
#          encounters from the analytic dataset produced in analytic_table.sas
# Outputs: None saved; produces the `analytic` dataframe in-session.
# =============================================================================

# ---- Setup ----
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here,
               tidyverse,
               lme4,
               data.table,
               lightgbm) #Loads necessary packages + installs if needed

here::i_am("scripts/12_mixed_effects_model.R") #Ensures `here` properly IDs top-level directory
set.seed(839)

# ---- Read in and prepare data ----
analytic_import <- read_csv(here("data", "analytic_2015_2018.csv"))

analytic <- analytic_import |>
  janitor::clean_names() |>
  mutate(
    msa     = factor(msa),
    year    = factor(year),
    sex     = factor(sex),
    eestatu = factor(eestatu),
    eeclass = factor(eeclass),
    emprel  = factor(emprel),
    indstry = factor(indstry),
    planyp  = factor(plantyp)
  )

# ---- Split into train/val by enrollee (80:20), keeping enrollee-weeks together ----
analytic <- as.data.table(analytic)

enrolids  <- unique(analytic$enrolid)
n_train   <- floor(0.8 * length(enrolids))
train_ids <- sample(enrolids, n_train)

train <- analytic[enrolid %in% train_ids]
val   <- analytic[!(enrolid %in% train_ids)]

# ---- Prep for use with LightGBM ----
# Drop outcomes, identifiers, or other nonsense variables
drop_cols <- c("enrolid", "efamid", "agegrp", "dobyr", "dtstart", "dtend", 
               "memdays", "msa", "year", "date", "enrolled", "ed7","ip7",
               "ervis", "ipadm")

features <- setdiff(names(train), drop_cols)

# Need to coerce factors into integers
  train[, ..features] |>
    mutate(across(where(is.factor), as.integer)) |>
    as.matrix()

# And tell the model what is categorical
cat_features <- which(sapply(train[, ..features], is.factor)) - 1

# ---- Define models ----
# ED model
dtrain_ed <- lgb.Dataset(
  data                = as.matrix(train[, ..features] |> mutate(across(where(is.factor), as.integer))),
  label               = train$ed7,
  categorical_feature = cat_features,
  params              = list(feature_pre_filter = FALSE)
)

dval_ed <- lgb.Dataset(
  data                = as.matrix(val[, ..features] |> mutate(across(where(is.factor), as.integer))),
  label               = val$ed7,
  categorical_feature = cat_features,
  reference           = dtrain_ed
)

# IP model
dtrain_ip <- lgb.Dataset(
  data                = as.matrix(train[, ..features] |> mutate(across(where(is.factor), as.integer))),
  label               = train$ip7,
  categorical_feature = cat_features
)

dval_ip <- lgb.Dataset(
  data                = as.matrix(val[, ..features] |> mutate(across(where(is.factor), as.integer))),
  label               = val$ip7,
  categorical_feature = cat_features,
  reference           = dtrain_ip
)

# ---- Constants ----
ed_prevalence = mean(train$ed7, na.rm = TRUE)
ip_prevalence = mean(train$ip7, na.rm = TRUE)

# ---- Define parameter space ----
base_spw <- (1 - ed_prevalence) / ed_prevalence

param_grid <- expand.grid(
  num_leaves        = c(11, 13, 15, 17),
  min_data_in_leaf  = c(1000, 2000, 5000),
  lambda_l1         = c(0, 0.1, 1),
  lambda_l2         = c(0.9, 1, 1.1),
  min_gain_to_split = c(0.01, 0.1)
)

param_grid <- param_grid[sample(nrow(param_grid), 50), ]

# ---- CV to optimize parameters ----
results <- vector("list", nrow(param_grid))

for (i in seq_len(nrow(param_grid))) {
  params_i <- list(
    objective          = "binary",
    metric             = c("average_precision"),
    learning_rate      = 0.05,
    max_depth          = -1,
    feature_pre_filter = FALSE,
    num_leaves         = param_grid$num_leaves[i],
    min_data_in_leaf   = param_grid$min_data_in_leaf[i],
    scale_pos_weight   = base_spw,
    lambda_l1          = param_grid$lambda_l1[i],
    lambda_l2          = param_grid$lambda_l2[i],
    min_gain_to_split  = param_grid$min_gain_to_split[i],
    feature_fraction   = 0.8,
    bagging_fraction   = 0.8,
    bagging_freq       = 5,
    verbose            = -1
  )
  
  cv_result <- lgb.cv(
    params    = params_i,
    data      = dtrain_ed,
    nfold     = 5,
    nrounds   = 2000,
    early_stopping_rounds = 50,
    eval_freq = 999,
    showsd    = TRUE
  )
  
  best_prauc   <- max(unlist(cv_result$record_evals$valid$average_precision$eval))
  best_nrounds <- which.max(unlist(cv_result$record_evals$valid$average_precision$eval))
  
  
  results[[i]] <- tibble(
    iteration         = i,
    num_leaves        = param_grid$num_leaves[i],
    min_data_in_leaf  = param_grid$min_data_in_leaf[i],
    scale_pos_weight  = param_grid$scale_pos_weight[i],
    lambda_l1         = param_grid$lambda_l1[i],
    lambda_l2         = param_grid$lambda_l2[i],
    min_gain_to_split = param_grid$min_gain_to_split[i],
    best_nrounds      = best_nrounds,
    cv_prauc          = best_prauc
  )
  
  if (i %% 10 == 0) cat("Completed", i, "of", nrow(param_grid), "\n" )
}

# put results of loop in df
results_df <- bind_rows(results) |> 
  arrange(desc(cv_prauc))

cat("\nTop 10 combinations:\n")
print(head(results_df, 10))

# ---- Train model ----
# refit on full training set with best parameters
best <- results_df[1, ]

params_final <- list(
  objective          = "binary",
  metric             = "binary_logloss",
  learning_rate      = 0.05,
  max_depth          = -1,
  num_leaves         = 13,
  min_data_in_leaf   = 20,
  scale_pos_weight   = 10,
  lambda_l1          = 0,
  lambda_l2          = 1.1,
  feature_pre_filter = FALSE,
  min_gain_to_split  = 0.01,
  feature_fraction   = 0.8,
  bagging_fraction   = 0.8,
  bagging_freq       = 5,
  verbose            = 1
)


params_default <- list(
  objective = "binary",
  metric    = "binary_logloss", 
  verbose   = 1
)
# ---- Train models ----
model_ed <- lgb.train(
  params    = params_default,
  data      = dtrain_ed, 
  nrounds   = 2000,
  valids    = list(val = dval_ed),
  early_stopping_rounds = 50,
  eval_freq = 25
)

lgb.save(model_ed, here("data", "model_ed.lgb"))

model_ip <- lgb.train(
  params    = params_final,
  data      = dtrain_ip, 
  nrounds   = best$best_nrounds,
)

lgb.save(model_ip, here("data", "model_ip.lgb"))

# ---- Evaluate ----
pred_ed <- predict(model_ed, as.matrix(val[, ..features] |> 
                                         mutate(across(where(is.factor), as.integer))))

pr_ed <- pr.curve(
  scores.class0 = pred_ed[val$ed7 == 1],
  scores.class1 = pred_ed[val$ed7 == 0],
  curve         = TRUE
)

roc_ed <- roc(val$ed7, pred_ed)

cat("ED Model - PR-AUC:", pr_ed$auc.integral, "\n")
cat("ED Model - ROC-AUC:", auc(roc_ed), "\n")
cat("PR-AUC to beat:", mean(val$ed7, na.rm = TRUE), "\n")