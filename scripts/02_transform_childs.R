#### Setup ####
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               lubridate,
               sf) #Loads necessary packages + installs if needed

here::i_am("scripts/02_transform_childs.R") #Ensures `here` properly IDs top-level directory

#### Load data ####
us_smokedays <- readRDS(here("data", "us_smokedays.rds"))

#### Filter to WA smoke days in study period (2021) ####
us_smokedays$GEOID10 <- as.numeric(us_smokedays$GEOID10)
# ZIPs in WA begin with 980 - 994
wa_smokedays <- us_smokedays |>
  filter(GEOID10 >= 98000 & GEOID10 <= 99499) |>
  filter(year(date) >= 2015 & year(date) <= 2020)

#### Save Childs smoke days dataframe ####
saveRDS(wa_smokedays, file = here("data", "wa_smokedays_15_20.rds"))
