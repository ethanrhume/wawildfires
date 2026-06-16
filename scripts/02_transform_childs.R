# =============================================================================
# Script:  02_transform_childs.R
# Author:  Ethan Hume
# Date:    2026-06-10
# Inputs:  1. RDS file of US ZCTA-day smoke PM2.5 predictions
#             (data/us_smokedays.rds) produced by 01_clean_childs.R.
# Purpose: Filters the national smoke day dataset to Washington State ZCTAs
#          and the 2015–2020 study period.
# Outputs: 1. RDS file of WA ZCTA-day smoke predictions
#             (data/wa_smokedays_15_20.rds) used by downstream scripts.
# Note:    This script is separate from the previous due to the high memory 
#          demand in 01_clean_childs.R. I wanted to be able to load 
#          us_smokedays.RDS directly without constructing again if needed, even 
#          if the study window or population needed adjustment.
# =============================================================================

# ---- Setup ----
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here,
               tidyverse,
               lubridate,
               sf) #Loads necessary packages + installs if needed

here::i_am("scripts/02_transform_childs.R") #Ensures `here` properly IDs top-level directory

# ---- Load data ----
us_smokedays <- readRDS(here("data", "us_smokedays.rds"))

# ---- Filter to WA smoke days in study period (2015-2020) ----
us_smokedays$GEOID10 <- as.numeric(us_smokedays$GEOID10)
# ZIPs in WA begin with 980 - 994
wa_smokedays <- us_smokedays |>
  filter(GEOID10 >= 98000 & GEOID10 <= 99499) |>
  filter(year(date) >= 2015 & year(date) <= 2020)

# ---- Save Childs smoke days dataframe ----
saveRDS(wa_smokedays, file = here("data", "wa_smokedays_15_20.rds"))
