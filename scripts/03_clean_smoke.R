#### Setup ####
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               lubridate,
               sf,
               purrr,
               janitor,
               snakecase) #Loads necessary packages + installs if needed

here::i_am("scripts/03_clean_smoke.R") #Ensures `here` properly IDs top-level directory

#### Load in monitor data ####
# create vector of filepaths based on naming convention
raw_monitor_data <- list.files(path = here("data", "daily_88502_pm"), 
                               pattern = "us_20[12][0-9]\\.csv", 
                               full.names = TRUE) 

# remove ".csv" for names of dfs once read into R
names(raw_monitor_data) <- basename(raw_monitor_data) |> str_remove("\\.csv")

# create list with each df named as above
data_list <- map(raw_monitor_data, read_csv)

#### Clean monitor data ####
# list-wise, filter to WA for monitors that exist in 2020

# define the longitudes of the extant monitors in 2020
target_longitudes <- unique(data_list$us_2020$Longitude)

# cleaning function
clean_raw_files <- function(df) {
  df |>
    clean_names() |> 
    mutate(date = as.Date(date_local),
           month_date = format(date, "%m-%d"),
           year = year(date)) |>
    filter(state_code == 53, # WA FIPS code is 53
           longitude %in% target_longitudes,
           observation_count == 1, 
           sample_duration != "1 HOUR") |>
    select(date, month_date, year, arithmetic_mean, site_num, local_site_name, latitude, 
           longitude, address, poc) 
}

# apply cleaning function to df list
cleaned_list <- map(data_list, clean_raw_files)

# combine df list into one df for all observations
wa_monitors <- bind_rows(cleaned_list)

# standardize site names to 2020 versions
name_lookup <- wa_monitors |>
  group_by(longitude) |>
  filter(year == max(year)) |>
  slice(1) |>
  select(longitude, standard_name = local_site_name)

wa_monitors <- wa_monitors |>
  left_join(name_lookup, by = "longitude") |>
  mutate(local_site_name = standard_name) |>
  select(-standard_name)

#' find the POC with the most observations per site -- needed since multiple POC 
#' on some sites, which will affect sd's calculated later
poc_winners <- wa_monitors |>
  count(longitude, poc) |>            # Count rows per site per POC
  group_by(longitude) |>
  slice_max(n, n = 1, with_ties = FALSE) |> # Keep only the POC with the highest 'n'
  select(longitude, winning_poc = poc)

# filter the wa_monitors df to only keep the poc_winners
wa_monitors <- wa_monitors |>
  inner_join(poc_winners, by = "longitude") |>
  filter(poc == winning_poc) |>
  select(-winning_poc, -poc) # remove helper column

# assign a unique site_num to each monitor, as EPA site_num is arbitrary
# create a ref table of unique longitudes so each gets a sequential ID
site_lookup <- wa_monitors |>
  distinct(longitude) |>
  arrange(longitude) |>
  mutate(new_site_num = row_number())

# join new IDs back to original df
wa_monitors <- wa_monitors |>
  left_join(site_lookup, by = "longitude") |>
  mutate(site_num = new_site_num) |>
  select(-new_site_num)

#### clean aqs monitors ####
aqs_monitors <- read_csv(here("data", "monitor_assignments", "aqs_monitors.csv"))
aqs_monitors <- aqs_monitors |>
  clean_names() |>
  filter(longitude %in% wa_monitors$longitude,
         parameter_code == 88502,
         last_sample_date >= as.Date("2010-01-01"))

#' find the POC with the most observations per site -- needed since multiple POC 
#' on some sites is creating duplicates in the aqs_monitors df
aqs_poc_winners <- aqs_monitors |>
  count(longitude, poc) |>            # Count rows per site per POC
  group_by(longitude) |>
  slice_max(n, n = 1, with_ties = FALSE) |> # Keep only the POC with the highest 'n'
  select(longitude, winning_poc = poc)

# filter the aqs_monitors df to only keep the poc_winners
aqs_monitors <- aqs_monitors |>
  inner_join(aqs_poc_winners, by = "longitude") |>
  filter(poc == winning_poc) |>
  select(-winning_poc, -poc) |> # remove helper column  
  select(latitude, longitude, local_site_name, address)

#' write aqs_monitors df to csv. this will be used to assign monitors to population
#' weighted centroids for ZCTAs and MSAs in ArcGIS Pro
write_csv(aqs_monitors, file = here("data", "monitor_assignments", 
                                    "monitors_clean.csv"))

#### Save clean WA monitor data 2010 - 2020 ####
# write cleaned df to new file
saveRDS(wa_monitors, file = here("data", "wa_monitors_clean.rds"))
