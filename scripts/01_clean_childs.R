# =============================================================================
# Script:  01_clean_childs.R
# Author:  Ethan Hume
# Date:    2026-06-10
# Inputs:  1. Daily ZCTA-level PM2.5 predictions on smoke days from Childs 
#             et al. (2022).
#          2. A ZCTA shapefile.
# Purpose: Joins Childs' smoke day predictions  to the full set of ZCTA-days 
#          (2006–2020), retaining NAs for non-smoke days rather than imputing 
#          zero. 
# Outputs: 1. A single RDS file (data/us_smokedays.rds) used by
#          downstream scripts.
# =============================================================================

# ---- Setup ----
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(here, 
               tidyverse,
               lubridate,
               sf) #Loads necessary packages + installs if needed

here::i_am("scripts/01_clean_childs.R") #Ensures `here` properly IDs top-level directory

# ---- Load Childs et al. (2022) data ----
#' Load smokePM predictions on smoke days
#' This is ZCTA5 level smoke days and associated predictions from 
#' January 1, 2006 to December 31, 2020 in the entire contiguous US. Non-smoke
#' days are not included in this file.

preds <- readRDS(here("data", "zcta",
                     "smokePM2pt5_predictions_daily_zcta_20060101-20231231.rds"))
names(preds) <- c("GEOID10", "date", "smokePM_pred")
# Load ZCTAs
zctas = read_sf("./data/zcta/tl_2019_us_zcta510")

# Load full set of dates
dates = seq.Date(ymd("20060101"), ymd("20201231"), by = "day")

# ---- Combine dfs together, save ----
# Get full combination of ZCTA-days
# Warning: this may require a large amount of memory
out = expand.grid(GEOID10 = zctas$GEOID10, date = dates)

# Match smokePM predictions on smoke days to ZCTA-days
out = left_join(out, preds, by = c("GEOID10", "date"))

# Write to a file that can be stored and loaded later
saveRDS(out, file = here("data", "us_smokedays.rds"))