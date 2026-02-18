#### Setup ####
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               lubridate,
               tidyverse,
               sf) #Loads necessary packages + installs if needed

here::i_am("scripts/00_explore.R") #Ensures `here` properly IDs top-level directory
source(here("R", "custom_functions.R")) #Adds custom functions to environment

#### Childs lab data load and combine####
preds <- readRDS(here("data", "childs_pm_2006_2020.rds"))
zctas <- read_sf(here("data", "2019_us_zcta"))
dates <- seq.Date(ymd("20060101"), ymd("20201231"), by = "day")

# Get full combination of ZCTA-days
# Warning: this may require a large amount of memory
out = expand.grid(GEOID10 = zctas$GEOID10, date = dates)


# Match smokePM predictions on smoke days to ZCTA-days
out = left_join(out, preds, by = c("GEOID10", "date"))


# Predict 0 for remaining ZCTA-days, which are non-smoke days
out = mutate(out, smokePM_pred = replace_na(smokePM_pred, 0))



#### General load data ####
# Load in Washington PM2.5 data for 2023 from EPA
wa2023 <- read_csv(here("data", "wa_2023.csv"))

# Start by filtering just to Ritzville monitor for now
ritzville <- wa2023 |>
  filter(
    `Site ID` == 530010003
  )

# Also filter to non zero PM days
non_zero_wa <- wa2023 |>
  filter(
    `Daily Mean PM2.5 Concentration` != 0
  )

# What proportion of Washington days had 0 PM
1 - (length(non_zero_wa$`Daily Mean PM2.5 Concentration`) / length(wa2023$`Daily Mean PM2.5 Concentration`))
# Basic histograms of PM2.5 
hist(ritzville$`Daily Mean PM2.5 Concentration`, breaks = 100)
hist(wa2023$`Daily Mean PM2.5 Concentration`, breaks = 100)
hist(non_zero_wa$`Daily Mean PM2.5 Concentration`, breaks = 100)

# Clean to see catefories
names(wa2023) <- tosnake(names(wa2023))
names(wa2023)[names(wa2023) == "daily_mean_pm2_5_concentration"] <- "daily_pm"
names(wa2023)[names(wa2023) == "daily_aqi_value"] <- "daily_aqi"

keepers <- c("date", "source", "site_id", "poc", "daily_pm", "daily_aqi", 
             "local_site_name", "site_latitude", "site_longitude")
wa2023 <- wa2023 |>
  select(all_of(keepers))

wa2023 <- wa2023 |>
  mutate(aqi_cat = case_when(
    daily_aqi < 72 ~ "None",
    daily_aqi >= 72 & daily_aqi < 101 ~ "Employer encouraged to provide N95",
    daily_aqi >= 101 & daily_aqi < 351 ~ "Employer must provide and encourage N95",
    daily_aqi >= 351 & daily_aqi < 849 ~ "Employer must distribute and encourage N95",
    daily_aqi >= 849 & daily_aqi < 958 ~ "N95 equivalent or better required",
    daily_aqi >= 957 ~ "P100 equivalent or better required",
    TRUE ~ NA_character_
  )) |>
  mutate(exp_cat = case_when(
    daily_aqi < 73 ~ "None",
    daily_aqi >= 73 & daily_aqi < 101 ~ "Encouraged",
    daily_aqi >= 101 ~ "Required",
    TRUE ~ NA_character_
  )) |>
  mutate(epa_cat = case_when(
    daily_aqi < 51 ~ "Good",
    daily_aqi >= 51 & daily_aqi < 101 ~ "Moderate",
    daily_aqi >= 101 & daily_aqi < 151 ~ "Unhealthy for Sensitive Groups",
    daily_aqi >= 151 & daily_aqi < 201 ~ "Unhealthy",
    daily_aqi >= 201 & daily_aqi < 301 ~ "Very Unhealthy",
    daily_aqi > 301 ~ "Hazardous",
  )) |>
  mutate(or_cat = case_when(
    daily_aqi < 101 ~ "None",
    daily_aqi >= 101 & daily_aqi < 277 ~ "Employer must provide N95 for voluntary use",
    daily_aqi >= 277 & daily_aqi < 849 ~ "Respirator mandatory use, no fit test",
    daily_aqi > 849 ~ "Respirator mandatory use, fit test",
  )) |>
  mutate(who_cat = case_when(
    daily_pm <= 15 ~ "Good",
    daily_pm > 15 ~ "Poor"
  ))

table(wa2023$epa_cat)

#histograms of aqi

hist(wa2023$daily_aqi[wa2023$daily_aqi != 0])

summary(wa2023$daily_aqi)
summary(wa2023$daily_pm)
summary(ritzville$`Daily Mean PM2.5 Concentration`)

# How much of the data is below Pm 12 AQI 50 Moderate
length(wa2023$daily_pm[wa2023$daily_pm < 12]) / length(wa2023$daily_pm)
# yields 91.04% of data "Good" AQI

#What would my cutoff for smoke days be with mean + 1 sd for ritzville

ritz_mean <- mean(wa2023$daily_pm[wa2023$site_id == 530010003])
ritz_sd <- sd(wa2023$daily_pm[wa2023$site_id == 530010003])
ritz_cut <- ritz_mean + ritz_sd
ritz_cut #19.72, AQI 66, moderate

#What would my cutoff for smoke days be with 75th percentile for ritzville
quantile(wa2023$daily_pm[wa2023$site_id == 530010003], prob = 0.75, na.rm = T)
# yields 5.6, AQI 23, good

#### Based on above calculations, we would want to use mean + 1 sd.
#### But what if we were taking into account the month?
names(ritzville) <- tosnake(names(ritzville))
names(ritzville)[names(ritzville) == "daily_mean_pm2_5_concentration"] <- "daily_pm"
names(ritzville)[names(ritzville) == "daily_aqi_value"] <- "daily_aqi"

keepers <- c("date", "source", "site_id", "poc", "daily_pm", "daily_aqi", 
             "local_site_name", "site_latitude", "site_longitude")
ritzville <- ritzville |>
  select(all_of(keepers))

ritzville <- ritzville |>
  mutate(aqi_cat = case_when(
    daily_aqi < 72 ~ "None",
    daily_aqi >= 72 & daily_aqi < 101 ~ "Employer encouraged to provide N95",
    daily_aqi >= 101 & daily_aqi < 351 ~ "Employer must provide and encourage N95",
    daily_aqi >= 351 & daily_aqi < 849 ~ "Employer must distribute and encourage N95",
    daily_aqi >= 849 & daily_aqi < 958 ~ "N95 equivalent or better required",
    daily_aqi >= 957 ~ "P100 equivalent or better required",
    TRUE ~ NA_character_
  )) |>
  mutate(exp_cat = case_when(
    daily_aqi < 73 ~ "None",
    daily_aqi >= 73 & daily_aqi < 101 ~ "Encouraged",
    daily_aqi >= 101 ~ "Required",
    TRUE ~ NA_character_
  )) |>
  mutate(epa_cat = case_when(
    daily_aqi < 51 ~ "Good",
    daily_aqi >= 51 & daily_aqi < 101 ~ "Moderate",
    daily_aqi >= 101 & daily_aqi < 151 ~ "Unhealthy for Sensitive Groups",
    daily_aqi >= 151 & daily_aqi < 201 ~ "Unhealthy",
    daily_aqi >= 201 & daily_aqi < 301 ~ "Very Unhealthy",
    daily_aqi > 301 ~ "Hazardous",
  )) |>
  mutate(or_cat = case_when(
    daily_aqi < 101 ~ "None",
    daily_aqi >= 101 & daily_aqi < 277 ~ "Employer must provide N95 for voluntary use",
    daily_aqi >= 277 & daily_aqi < 849 ~ "Respirator mandatory use, no fit test",
    daily_aqi > 849 ~ "Respirator mandatory use, fit test",
  )) |>
  mutate(who_cat = case_when(
    daily_pm <= 15 ~ "Good",
    daily_pm > 15 ~ "Poor"
  ))

ggplot() +
  geom_line(data = wa2023,
            aes(x = date,
                y = daily_pm))

pm25_clean <- read_csv(here("data", "pm25_clean.csv"))

yakima <- pm25_clean |>
  filter(site_id == 530770005) |>
  mutate(
    date = mdy(date)
  )

ggplot() +
  geom_line(data = pm25_clean,
            aes(x = date,
                y = daily_pm))

ggplot() +
  geom_line(data = yakima,
            aes(x = date,
                y = daily_pm)) +
  geom_smooth(data = yakima,
              aes(x = date,
                  y = daily_pm))



#### Load and clean US EPA monitor data ####

epa_monitors_raw <- read_csv(here("data", "aqs_monitors.csv"))

names(epa_monitors_raw) <- tosnake(names(epa_monitors_raw))

epa_monitors <- epa_monitors_raw |>
  filter(
    state_name == "Washington",
    last_sample_date > as.Date("2020-01-01"),
    str_detect(parameter_name, "PM2\\.5"))

epa_monitors <- epa_monitors |>
  arrange(latitude, desc(last_sample_date)) |>
  group_by(latitude) |>
  slice(1) |>
  ungroup()

epa_monitors_loc <- epa_monitors |>
  select(
    latitude,
    longitude,
    local_site_name,
    address,
    pqao
  )

write_csv(epa_monitors, here("data", "air_monitors.csv"))
write_csv(epa_monitors_loc, here("data", "air_monitors_loc.csv"))


## Testing age^2 will turn parabolic data to linear
n <- 1000
x <- seq(0, 100, length.out = n)   # span around the vertex
y <- (x - 45)^2                  # shifted parabola
df <- data.frame(X = x, Y = y)
head(df)

plot(df)

df2 <- data.frame(X = (x - 45)^2, Y = y)

plot(df2)
