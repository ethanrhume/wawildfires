# =============================================================================
# Script:  05_aim1_analysis.R
# Author:  Ethan Hume
# Date:    2026-06-10
# Inputs:  1. WA ZCTA-day smoke PM2.5 predictions from Childs et al.
#             (data/wa_smokedays_15_20.rds) from 02_transform_childs.R.
#          2. Constructed ZCTA-day smoke indicators
#             (data/zcta_smokedays_constructed.rds) from 04_transform_smoke.R.
#          3. Constructed MSA-day smoke indicators
#             (data/msa_smokedays_constructed.rds) from 04_transform_smoke.R.
#          4. WA ZCTA shapefile (data/zcta520/).
# Purpose: Validates the constructed smoke day indicators (Aim 1) against the
#          Childs et al. smoke PM2.5 predictions as a reference standard.
#          Computes overall and ZCTA-level classification metrics (sensitivity,
#          specificity, F1, MCC, accuracy, precision) and maps results
#          spatially across Washington ZCTAs.
# Outputs: None saved yet (write_csv() call is incomplete).
# =============================================================================

# ---- Setup ----
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here,
               tidyverse,
               lubridate) #Loads necessary packages + installs if needed

here::i_am("scripts/05_aim1_analysis.R") #Ensures `here` properly IDs top-level directory

# ---- Load in data ----
childs_smokedays_pre <- readRDS(file = here("data", "wa_smokedays_15_20.rds"))
zcta_smokedays <- readRDS(file = here("data", "zcta_smokedays_constructed.rds"))
msa_smokedays <- readRDS(file = here("data", "msa_smokedays_constructed.rds"))

# ---- Standardize Childs smoke days to constructed df format ----
childs_smokedays <- childs_smokedays_pre |>
  mutate(
    month_date = format(date, "%m-%d"),
    month = month(date),
    year = year(date),
    smokePM_pred = round(smokePM_pred, digits = 1), #important transformation of Childs data
    smoke = case_when(
      is.na(smokePM_pred) ~ FALSE,
      smokePM_pred <= 9.1 ~ FALSE,
      smokePM_pred > 9.1 ~ TRUE
#      !is.na(smokePM_pred) ~ TRUE
    ),
  ) |>
  rename(zcta = GEOID10, pred = smokePM_pred) |>
  select(zcta, month_date, month, year, pred, smoke) |>
  arrange(year, zcta, month_date)

# ---- Filter constructed smoke day dfs to 2015+ for analysis ----
msa_smokedays <- msa_smokedays |>
  filter(year >= 2015)

zcta_smokedays <- zcta_smokedays |>
  filter(year >= 2015)

# ---- Make a shared truth vs. constructed df for ZCTA ----
zcta_analysis <- inner_join(x = zcta_smokedays,
                            y = childs_smokedays,
                            by = c("zcta", "month_date", "month", "year"),
                            relationship = "one-to-one") |>
  rename(constr_smoke = smoke.x, true_smoke = smoke.y) |>
  mutate(
    confusion = case_when(
      constr_smoke == TRUE & true_smoke == TRUE ~ "TP",
      constr_smoke == TRUE & true_smoke == FALSE ~ "FP",
      constr_smoke == FALSE & true_smoke == TRUE ~ "FN",
      constr_smoke == FALSE & true_smoke == FALSE ~ "TN"
    )
  ) |>
  filter(!is.na(constr_smoke)) # not all monitors reported every day, don't include in analysis

# ---- Calculate overall sensitivity and specificity ----
true_pos = as.numeric(sum(zcta_analysis$confusion == "TP"))
false_pos = as.numeric(sum(zcta_analysis$confusion == "FP"))
true_neg = as.numeric(sum(zcta_analysis$confusion == "TN"))
false_neg = as.numeric(sum(zcta_analysis$confusion == "FN"))

sensitivity = true_pos / (true_pos + false_neg + 0.00001)
specificity = true_neg / (true_neg + false_pos + 0.00001)
f1 = (true_pos) / (true_pos + (0.5 * (false_pos + false_neg)) + 0.00001)
mcc = ((true_pos * true_neg) - (false_pos * false_neg)) / (sqrt((true_pos + false_pos)*(true_pos + false_neg)*(true_neg + false_pos)*(true_neg + false_neg)) + 0.00001)
accuracy = (true_pos + true_neg) / (nrow(zcta_analysis))
precision = true_pos / (true_pos + false_pos + 0.00001)

# ---- Calculate ZCTA-level sensitivity and specificity ----
zcta_level_analysis <- data.frame(zcta = unique(zcta_analysis$zcta))

zcta_level_analysis <- zcta_analysis |>
  group_by(zcta) |>
  summarise(
    true_pos = as.numeric(sum(confusion == "TP")),
    false_pos = as.numeric(sum(confusion == "FP")),
    true_neg = as.numeric(sum(confusion == "TN")),
    false_neg = as.numeric(sum(confusion == "FN")),
    sensitivity = true_pos / (true_pos + false_neg + 0.00001),
    specificity = true_neg / (true_neg + false_pos + 0.00001),
    f1 = (true_pos) / (true_pos + (0.5 * (false_pos + false_neg)) + 0.00001),
    mcc = ((true_pos * true_neg) - (false_pos * false_neg)) / (sqrt((true_pos + false_pos)*(true_pos + false_neg)*(true_neg + false_pos)*(true_neg + false_neg)) + 0.00001),
    accuracy = (true_pos + true_neg) / (true_pos + true_neg + false_pos + false_neg + 0.00001),
    precision = true_pos / (true_pos + false_pos + 0.00001)
  ) |>
  ungroup()

summary(zcta_level_analysis)






# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr) # For data manipulation if needed
library(tigris)
library(PNWColors)
library(NatParksPalettes)

# 1. Locate the .shp file
# This searches your 'census_shape' folder for a file ending in .shp
shapefile_path <- list.files(path = here("data", "zcta520"), 
                             pattern = "\\.shp$", 
                             full.names = TRUE)

# Check if a file was found
if (length(shapefile_path) == 0) {
  stop("No .shp file found in the 'census_shape' folder.")
} else {
  print(paste("Loading:", shapefile_path[1]))
}

# 2. Read the Shapefile
zcta_data <- st_read(shapefile_path[1])

# 3. Quick Visual Check
# If this is indeed Washington, you should see the state outline formed by Zip Codes.
# Note: Rendering might take a moment if the file is large.
ggplot(data = zcta_data) +
  geom_sf(fill = "lightblue", color = "white", size = 0.1) +
  theme_minimal() +
  labs(title = "Shapefile Verification: Washington ZCTAs",
       subtitle = paste("Loaded", nrow(zcta_data), "ZCTA features"))

zcta_level_analysis$zcta <- as.character(zcta_level_analysis$zcta)

zcta_data_joined <- zcta_data %>%
  left_join(zcta_level_analysis, by = c("ZCTA5CE" = "zcta"))

pal <- rev(natparks.pals("Olympic", 10, type = "continuous"))

ggplot(data = zcta_data_joined) +
  geom_sf(aes(fill = precision), color = NA) + 
  scale_fill_gradientn(
    colors = pal,
    name = "Precision",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1.0),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.4, "cm")
  )

ggplot(data = zcta_data_joined) +
  geom_sf(aes(fill = f1), color = NA) + 
  scale_fill_gradientn(
    colors = pal,
    name = "F1",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1.0),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.4, "cm")
  )

write_csv()