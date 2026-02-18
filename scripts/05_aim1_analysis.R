#### Setup ####
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               lubridate) #Loads necessary packages + installs if needed

here::i_am("scripts/05_aim1_analysis.R") #Ensures `here` properly IDs top-level directory

#### Load in data ####
childs_smokedays_pre <- readRDS(file = here("data", "wa_smokedays_15_20.rds"))
zcta_smokedays <- readRDS(file = here("data", "zcta_smokedays_constructed.rds"))
msa_smokedays <- readRDS(file = here("data", "msa_smokedays_constructed.rds"))

#### standardize childs_smokedays to how the constructed dfs look ####
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

#### filter constructed smokedays dfs to 2015+ for analysis ####
msa_smokedays <- msa_smokedays |>
  filter(year >= 2015)

zcta_smokedays <- zcta_smokedays |>
  filter(year >= 2015)

#### make a shared truth vs constructed df for zcta ####
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

#### Calculate overall sensitivity and specificity
true_pos = as.numeric(sum(zcta_analysis$confusion == "TP"))
false_pos = as.numeric(sum(zcta_analysis$confusion == "FP"))
true_neg = as.numeric(sum(zcta_analysis$confusion == "TN"))
false_neg = as.numeric(sum(zcta_analysis$confusion == "FN"))

sensitivity = true_pos / (true_pos + false_neg)
specificity = true_neg / (true_neg + false_pos)
f1 = (true_pos) / (true_pos + (0.5 * (false_pos + false_neg)) )
mcc = ((true_pos * true_neg) - (false_pos * false_neg)) / (sqrt((true_pos + false_pos)*(true_pos + false_neg)*(true_neg + false_pos)*(true_neg + false_neg)))
accuracy = (true_pos + true_neg) / (nrow(zcta_analysis))
precision = true_pos / (true_pos + false_pos)

#### Calculate zip specific sensitivity and specificity
zcta_level_analysis <- data.frame(zcta = unique(zcta_analysis$zcta))

zcta_level_analysis <- zcta_analysis |>
  group_by(zcta) |>
  summarise(
    true_pos = as.numeric(sum(confusion == "TP")),
    false_pos = as.numeric(sum(confusion == "FP")),
    true_neg = as.numeric(sum(confusion == "TN")),
    false_neg = as.numeric(sum(confusion == "FN")),
    sensitivity = true_pos / (true_pos + false_neg),
    specificity = true_neg / (true_neg + false_pos),
    f1 = (true_pos) / (true_pos + (0.5 * (false_pos + false_neg))),
    mcc = ((true_pos * true_neg) - (false_pos * false_neg)) / (sqrt((true_pos + false_pos)*(true_pos + false_neg)*(true_neg + false_pos)*(true_neg + false_neg))),
    accuracy = (true_pos + true_neg) / (true_pos + true_neg + false_pos + false_neg),
    precision = true_pos / (true_pos + false_pos)
  ) |>
  ungroup()

summary(zcta_level_analysis)






# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr) # For data manipulation if needed
library(tigris)

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


library(sf)
library(ggplot2)
library(tigris) # Excellent package for fetching US Census borders

# 1. Clean the Join Keys (Crucial Step)
# Ensure both columns are characters to preserve leading zeros (e.g., "098")
# and trim any accidental whitespace.
zcta_level_analysis$zcta <- as.character(zcta_level_analysis$zcta)
zcta_data$ZCTA5CE <- trimws(as.character(zcta_data$ZCTA5CE)) # 'trimws' removes spaces

# 2. Join the Data
# We merge the dataframe INTO the shapefile.
# We use left_join on the shapefile to keep the geometry for the map.
zcta_data_joined <- zcta_data %>%
  left_join(zcta_level_analysis, by = c("ZCTA5CE" = "zcta"))

# 3. Plot the Gradient
ggplot(data = zcta_data_joined) +
  # Plot the ZCTAs, filling them based on the 'accuracy' column
  geom_sf(aes(fill = specificity), color = NA) + 
  
  # Use a nice color scale (Viridis is colorblind-friendly and distinct)
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "white", 
                       name = "Specificity") +
  
  # Clean up the look
  theme_void() +
  labs(title = "Washington State ZCTA Specificity Map",
       subtitle = "White areas indicate missing data or non-ZCTA regions")
