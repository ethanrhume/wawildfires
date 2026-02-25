#### Setup ####
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               lubridate,
               sf,
               purrr,
               janitor,
               snakecase,
               slider,
               tigris,
               sf) #Loads necessary packages + installs if needed

here::i_am("scripts/04_transform_smoke.R") #Ensures `here` properly IDs top-level directory


#### Load in data ####
wa_smokedays <- readRDS(here("data", "wa_monitors_clean.rds"))

zcta_nearest_monitors <- read_csv(here("data", "monitor_assignments", 
                                  "zcta_nearest_monitors.csv"))

zcta_pops_2011 <- read_csv(here("data", "census_pops", "2011_zcta_pop.csv"))
zcta_pops_2012 <- read_csv(here("data", "census_pops", "2012_zcta_pop.csv"))
zcta_pops_2013 <- read_csv(here("data", "census_pops", "2013_zcta_pop.csv"))
zcta_pops_2014 <- read_csv(here("data", "census_pops", "2014_zcta_pop.csv"))
zcta_pops_2015 <- read_csv(here("data", "census_pops", "2015_zcta_pop.csv"))
zcta_pops_2016 <- read_csv(here("data", "census_pops", "2016_zcta_pop.csv"))
zcta_pops_2017 <- read_csv(here("data", "census_pops", "2017_zcta_pop.csv"))
zcta_pops_2018 <- read_csv(here("data", "census_pops", "2018_zcta_pop.csv"))
zcta_pops_2019 <- read_csv(here("data", "census_pops", "2019_zcta_pop.csv"))
zcta_pops_2020 <- read_csv(here("data", "census_pops", "2020_zcta_pop.csv"))

zcta_pops <- list(zcta_pops_2011, zcta_pops_2012, zcta_pops_2013, zcta_pops_2014,
                  zcta_pops_2015, zcta_pops_2016, zcta_pops_2017, zcta_pops_2018, 
                  zcta_pops_2019, zcta_pops_2020)

zcta_msa_overlap <- read_csv(here("data", "monitor_assignments", 
                                  "zcta_msa_overlap.csv"))

#### Add active_fire variable ####
# load fires (point data from FIRMS MODIS)
us_fires <- read_csv(here("data", "fire_data", "us_fires.csv"))
can_fires <- read_csv(here("data", "fire_data", "can_fires.csv"))

us_fires <- us_fires |>
  filter(type == 0) |> # filters to presumed vegetation fires
  filter(confidence >= 50) # confidence over 50%

can_fires <- can_fires |>
  filter(type == 0) |>
  filter(confidence >= 50)

all_fires <- rbind(us_fires, can_fires)

# download WA boundary. dependcy = tigris package
wa_boundary <- states(cb = TRUE, year = 2020) |>
  filter(NAME == "Washington")

# project to Albers (EPSG:5070) for better area intersection (meter based)
wa_projected <- st_transform(wa_boundary, 5070)
fire_sf <- all_fires |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(5070)

# create 50 mi buffer zone (80,467 meters)
wa_buffer <- st_buffer(wa_projected, dist = 80467)

# calculate intersection
fire_sf <- st_filter(fire_sf, wa_buffer)

# pull dates of all active fires in WA
fires_active <- unique(fire_sf$acq_date)

# incorporate a binary active fire indicator to wa_smokedays
wa_smokedays <- wa_smokedays |>
  mutate(
    active_fire = case_when(
      date %in% fires_active ~ 1,
      TRUE ~ 0
    )
  )

#### Construct monitor-wise smoke days variable ####
#' Constructed smoke days will be defined as any day with active fire w/i 50 mi 
#' of WA state AND with EITHER daily average PM2.5 > 20 ug/ml OR daily average 
#' PM2.5 > 5-year rolling median of daily avg PM2.5 for that month

# need a month variable
wa_smokedays <- wa_smokedays |>
  mutate(
    month = substr(month_date, 1, 2)
  ) |>
  select(date, month_date, month, year, arithmetic_mean, site_num, local_site_name,
         latitude, longitude, address, active_fire)

#' Here we will start by calculating the monthly averages and SDs for each 
#' monitor's preceding five years of data.

# summarize daily data into monthly averages per site/year (less memory pressure)
monthly_summary <- wa_smokedays |>
  group_by(site_num, month, year) |>
  summarize(yr_mo_avg = mean(arithmetic_mean, na.rm = TRUE), .groups = "drop")

# calculate 5-year rolling mean for 2015+
rolling_history <- monthly_summary |>
  rename(target_year = year) |>
  # before 2015 we want NA in final product, so remove 'target' years before 2015
  filter(target_year >= 2015) |> 
  # join all historic yr_mo_avg to each target year_mo
  left_join(monthly_summary, by = c("site_num", "month"), 
            relationship = "many-to-many") |> 
  # remove those except the ones used to calculate rolling avg
  filter(year >= (target_year - 5) & year < target_year) |>
  # calculate rolling avg and sd, ungroup
  group_by(site_num, month, target_year) |>
  summarize(monthly_mean = mean(yr_mo_avg.y, na.rm = TRUE), 
            monthly_sd   = sd(yr_mo_avg.y, na.rm = TRUE),
            .groups = "drop")

# join back to the wa_smokedays
wa_smokedays <- wa_smokedays |>
  left_join(rolling_history, by = c("site_num", "month", "year" = "target_year"))

# now calculate 5 year rolling median for that month
wa_smokedays <- wa_smokedays |>
  # Ensure data is sorted so the rolling window looks at the correct years
  arrange(site_num, month, year) |>
  group_by(site_num, month) |>
  mutate(
    monthly_med = slide_index_dbl(
      .x = arithmetic_mean,
      .i = year,
      .f = ~median(.x, na.rm = TRUE),
      .before = 5, # look back 5 years
      .after = -1 # exclude the current year
    )
  ) |>
  ungroup()

# CONSTRUCT MONITOR-WISE SMOKE DAYS. Keep historic data in for use for MSA calcs
wa_smokedays <- wa_smokedays |>
  mutate(
    smoke = case_when(
      arithmetic_mean > 20 ~ TRUE,
      arithmetic_mean > monthly_med + 10 & active_fire == 1 ~ TRUE,
      TRUE ~ FALSE
    )
  )

#### Construct ZCTA-wise smoke day variable ####
# add population by year to zcta_nearest_monitors
zcta_nearest_monitors <- zcta_pops |>
  reduce(left_join, by = "zcta", .init = zcta_nearest_monitors)

# clean zcta_msa_overlap
zcta_msa_overlap <- zcta_msa_overlap |>
  mutate(msa = recode(msa, 
                      "Lewiston, ID-WA" = "lewiston",
                      "Kennewick-Richland, WA" = "kennewick_richland",
                      "Portland-Vancouver-Hillsboro, OR-WA" = "portland_vancouver_hillsboro",
                      "Spokane-Spokane Valley, WA" = "spokane_spokane_valley",
                      "Seattle-Tacoma-Bellevue, WA" = "seattle_tacoma_bellevue",
                      "Bellingham, WA" = "bellingham",
                      "Wenatchee-East Wenatchee, WA" = "wenatchee_east_wenatchee",
                      "Yakima, WA" = "yakima",
                      "Mount Vernon-Anacortes, WA" = "mount_vernon_anacortes",
                      "Olympia-Lacey-Tumwater, WA" = "olympia_lacey_tumwater",
                      "Longview-Kelso, WA" = "longview_kelso",
                      "Bremerton-Silverdale-Port Orchard, WA" = "bremerton_silverdale_port_orchard",
                      "Walla Walla, WA" = "walla_walla"
  ))


# bind to existing zcta_nearest_monitor df
zcta_nearest_monitors <- zcta_nearest_monitors |> 
  left_join(x = zcta_nearest_monitors,
            y = zcta_msa_overlap,
            by = NULL,
            relationship = "one-to-one"
  )

# need to ensure site_num is added corresponding with EPA monitor data
site_lookup <- wa_smokedays |>
  group_by(site_num) |>
  summarize(site = mean(site_num),
            long = mean(longitude)) |>
  mutate(site_num = site,
         longitude = long) |>
  select(-site, -long)

zcta_nearest_monitors <- left_join(x = zcta_nearest_monitors, 
                                   y = site_lookup, 
                                   by = NULL, 
                                   relationship = "many-to-one")

# save finalized product
saveRDS(zcta_nearest_monitors, file = here("data", "zcta_msa_monitor_clean.rds"))

# now to actually make the zcta-wise smokedays df
zctas <- unique(zcta_nearest_monitors$zcta)
all_days <- seq(as.Date("2011-01-01"), as.Date("2020-12-31"), by = "day")

zcta_smokedays_constructed <- expand.grid(zcta = zctas, 
                                          date = all_days)


zcta_smokedays_constructed <- zcta_smokedays_constructed |>
  mutate(
    month_date = format(date, "%m-%d"),
    month = month(date),
    year = year(date)) |>
  select(zcta, month_date, month, year) |>
  arrange(zcta, year, month, month_date)

site_lookup <- site_lookup |>
  left_join(x = site_lookup,
            y = zcta_nearest_monitors,
            by = NULL) |>
  select(zcta, site_num, latitude, longitude) |>
  arrange(zcta)

zcta_smokedays_constructed <- zcta_smokedays_constructed |>
  left_join(x = zcta_smokedays_constructed,
            y = site_lookup,
            by = NULL) |>
  select(-latitude, -longitude)

zcta_smokedays_constructed <- zcta_smokedays_constructed |>
  left_join(x = zcta_smokedays_constructed,
            y = wa_smokedays, 
            by = c("month_date", "year", "site_num")) |>
  clean_names() |>
  select(zcta, month_date, month_x, year, site_num, arithmetic_mean,
         monthly_mean, monthly_sd, monthly_med, smoke) |>
  rename(month = month_x)

# write finished product
write_rds(zcta_smokedays_constructed, here("data", "zcta_smokedays_constructed.rds"))







#### UNDER CONSTRUCTION: MSA SMOKE DAYS USE OUTDATED DEFINITION CURRENTLY ####

#' the following code adds the necessary components for a population weighted
#' average of monitors per MSA 

msa_smokeday_comps <- data.frame(zcta = zcta_nearest_monitors$zcta,
                                 msa = zcta_nearest_monitors$msa,
                                 pop_2011 = zcta_nearest_monitors$pop_2011,
                                 pop_2012 = zcta_nearest_monitors$pop_2012,
                                 pop_2013 = zcta_nearest_monitors$pop_2013,
                                 pop_2014 = zcta_nearest_monitors$pop_2014,
                                 pop_2015 = zcta_nearest_monitors$pop_2015,
                                 pop_2016 = zcta_nearest_monitors$pop_2016,
                                 pop_2017 = zcta_nearest_monitors$pop_2017,
                                 pop_2018 = zcta_nearest_monitors$pop_2018,
                                 pop_2019 = zcta_nearest_monitors$pop_2019,
                                 pop_2020 = zcta_nearest_monitors$pop_2020)

msa_smokeday_comps <- msa_smokeday_comps |> 
  group_by(msa) |> 
  mutate(denom_2011 = sum(pop_2011, na.rm = TRUE)) |> 
  mutate(denom_2012 = sum(pop_2012, na.rm = TRUE)) |> 
  mutate(denom_2013 = sum(pop_2013, na.rm = TRUE)) |> 
  mutate(denom_2014 = sum(pop_2014, na.rm = TRUE)) |> 
  mutate(denom_2015 = sum(pop_2015, na.rm = TRUE)) |> 
  mutate(denom_2016 = sum(pop_2016, na.rm = TRUE)) |> 
  mutate(denom_2017 = sum(pop_2017, na.rm = TRUE)) |> 
  mutate(denom_2018 = sum(pop_2018, na.rm = TRUE)) |> 
  mutate(denom_2019 = sum(pop_2019, na.rm = TRUE)) |> 
  mutate(denom_2020 = sum(pop_2020, na.rm = TRUE)) |> 
  ungroup()

msa_smokeday_comps <- msa_smokeday_comps |>
  left_join(x = msa_smokeday_comps,
            y = zcta_smokedays_constructed,
            by = c("zcta")) |>
  select(-monthly_mean, -monthly_sd, -smoke) |>
  mutate(
    pop = case_when(
      year == 2011 ~ pop_2011,
      year == 2012 ~ pop_2012,
      year == 2013 ~ pop_2013,
      year == 2014 ~ pop_2014,
      year == 2015 ~ pop_2015,
      year == 2016 ~ pop_2016,
      year == 2017 ~ pop_2017,
      year == 2018 ~ pop_2018,
      year == 2019 ~ pop_2019,
      year == 2020 ~ pop_2020
    ),
    denom = case_when(
      year == 2011 ~ denom_2011,
      year == 2012 ~ denom_2012,
      year == 2013 ~ denom_2013,
      year == 2014 ~ denom_2014,
      year == 2015 ~ denom_2015,
      year == 2016 ~ denom_2016,
      year == 2017 ~ denom_2017,
      year == 2018 ~ denom_2018,
      year == 2019 ~ denom_2019,
      year == 2020 ~ denom_2020
    )
  ) |>
  select(-starts_with("pop_"), -starts_with("denom_")) |>
  mutate(
    pop_con = pop*arithmetic_mean
  )

msa_smokeday_comps <- msa_smokeday_comps |>
  group_by(msa, month_date, year) |>
  mutate(numerator = sum(pop_con, na.rm = TRUE)) |>
  ungroup()

msa_smokeday_comps <- msa_smokeday_comps |>
  mutate(
    msa_con = numerator / denom
  )

# calculate rolling monthly averages and sd's
msa_smokedays_constructed <- msa_smokeday_comps |>
  select(msa, year, month, month_date, msa_con) |>
  distinct() |> # unique values for msa-days
  arrange(msa, month, year) |>
  group_by(msa, month, month_date) |> # create stacks of msa-months where we can examin between years
  mutate(
    monthly_mean = slide_dbl(msa_con, mean, .before = 4, .aafter = -1, 
                             .complete = TRUE, na.rm = TRUE),
    monthly_sd   = slide_dbl(msa_con, sd,   .before = 4, .after = -1,
                                   .complete = TRUE, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(msa, year, month)

# now calculate 5 year rolling median for that month
msa_smokedays_constructed <- msa_smokedays_constructed |>
  # Ensure data is sorted so the rolling window looks at the correct years
  arrange(msa, month, year) |>
  group_by(msa, month) |>
  mutate(
    monthly_med = slide_index_dbl(
      .x = msa_con,
      .i = year,
      .f = ~median(.x, na.rm = TRUE),
      .before = 5, # look back five years
      .after = -1 # exclude this year
    )
  ) |>
  ungroup()

# incorporate a binary active fire indicator to msa_smokedays_constructed
msa_smokedays_constructed <- msa_smokedays_constructed |>
  mutate(
    date = as.Date(paste(year, month_date, sep = "-")),
    active_fire = case_when(
      date %in% fires_active ~ 1,
      TRUE ~ 0
    )
  )

# now add the actual smokeday determination
msa_smokedays_constructed <- msa_smokedays_constructed |>
  mutate(
    smoke = case_when(
      msa_con > 20 ~ TRUE,
      msa_con > monthly_med + 10 & active_fire == 1 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# save final product
saveRDS(msa_smokedays_constructed, 
        file = here("data", "msa_smokedays_constructed.rds"))

  