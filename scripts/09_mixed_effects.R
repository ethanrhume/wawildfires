# =============================================================================
# Script:  09_mixed_effects.R
# Author:  Ethan Hume
# Date:    2026-06-10
# Inputs:  1. Analytic dataset (data/analytic_2015_2018.csv).
# Purpose: Fits a 3-level mixed-effects logistic regression (MSA, enrollee
#          nested in MSA, and year:week) examining the association between
#          smoke exposure and cardiorespiratory ED visits. Computes
#          intraclass correlation coefficients for the null model and
#          compares the null model to the full covariate-adjusted model.
# Outputs: None saved; model objects exist only in-session.
# =============================================================================

# ---- Setup ----
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               lme4) #Loads necessary packages + installs if needed

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

# ---- Build null model and find ICCs ----
null_model <- glmer(
  cr_ed_week ~ 1 +
    (1 | msa / enrolid) + (1 | year:week),
  data    = analytic,
  family  = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl   = list(maxfun = 2e5)
  )
)

summary(null_model)

#' ICC extraction since lme4 doesn't give it directly for 3 level model.
#' ICC explains what portion of variance is attributable to a given level of 
#' our hierarchical model

vc        <- as.data.frame(VarCorr(null_model))
v_msa     <- vc$vcov[vc$grp == "msa"]
v_enrolid <- vc$vcov[vc$grp == "enrolid:msa"]
v_week    <- vc$vcov[vc$grp == "year:week"]
v_resid   <- (pi^2) / 3 # This is the standard logistic residual variance

v_total <- v_msa + v_enrolid + v_week + v_resid

icc_msa     <- v_msa     / v_total
icc_enrolid <- v_enrolid / v_total
icc_week    <- v_week    / v_total

cat("ICC - MSA: ", round(icc_msa, 4), "\n")
cat("ICC - Enrollee: ", round(icc_enrolid, 4), "\n")
cat("ICC - Year:Week: ", round(icc_week, 4), "\n")

# ---- Full model ----

model <- glmer(
  cr_ed_week ~ smoke_days + smoke_days_lag1 + week + year + age + sex + eestatu +
    eeclass + emprel + indstry + plantyp + (1 | msa / enrolid) + (1 | year:week),
  data    = analytic,
  family  = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl   = list(maxfun = 2e5)
  )
)

summary (model)

