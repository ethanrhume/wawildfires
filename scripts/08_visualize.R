# =============================================================================
# Script:  08_visualize.R
# Author:  Ethan Hume
# Date:    2026-06-10
# Inputs:  1. Enrollee-day utilization and exposure dataset
#             (data/data2015.fst) from 10_smoke_v_util.R.
# Purpose: Visualizes ED, PCP, and outpatient utilization (all-cause and
#          cardiorespiratory-specific) against smoke exposure intervals
#          during the 2015 fire season, at both the MSA and statewide level.
# Outputs: None saved; plots are displayed only.
# =============================================================================

# ---- Setup ----
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               lubridate,
               haven,
               fst,
               lightgbm,
               data.table,
               PRROC,
               ggplot2,
               slider) #Loads necessary packages + installs if needed

here::i_am("scripts/11_visualize.R") #Ensures `here` properly IDs top-level directory
set.seed(839)

# ---- Read in prepared data ----
data2015 <- read_fst(here("data", "data2015.fst"))

# ---- Prep to plot at MSA level ----
plotting <- elig_op_smoke2 |>
  filter(date >= as.Date("2015-07-01") & date <= as.Date("2015-09-30")) |>
  group_by(msa, date) |>
  summarize(cr_ed_visits   = sum(cr_ed,     na.rm = TRUE),
            any_ed_visits  = sum(any_ed,    na.rm = TRUE),
            crac_ratio_ed  = sum(cr_ed,     na.rm = TRUE) / sum(any_ed,  na.rm = TRUE),
            cr_pcp_visits  = sum(cr_pcp,    na.rm = TRUE),
            any_pcp_visits = sum(any_pcp,   na.rm = TRUE),
            crac_ratio_pcp = sum(cr_pcp,    na.rm = TRUE) / sum(any_pcp, na.rm = TRUE),
            cr_op_visits   = sum(any_cr_op, na.rm = TRUE),
            any_op_visits  = sum(any_op,    na.rm = TRUE),
            crac_ratio_op  = sum(any_cr_op, na.rm = TRUE) / sum(any_op,  na.rm = TRUE),
            .groups = "drop") |>
  complete(
    msa, 
    date = seq(as.Date("2015-07-01"), as.Date("2015-09-30"), by = "day"),
    fill = list(cr_ed_visits = 0, any_ed_visits = 0, crac_ratio = 0)) |>
  group_by(msa) |>
  arrange(date, .by_group = TRUE) |>
  mutate(cr_ed_roll7   = slide_dbl(cr_ed_visits,  mean,   .before = 6, 
                                   .complete = FALSE),
         any_ed_roll7  = slide_dbl(any_ed_visits, mean,   .before = 6, 
                                   .complete = FALSE),
         crac_ed_roll7 = slide_dbl(crac_ratio_ed, mean,   .before = 6, 
                                   .complete = FALSE),
         cr_pcp_roll7   = slide_dbl(cr_pcp_visits,  mean, .before = 6, 
                                   .complete = FALSE),
         any_pcp_roll7  = slide_dbl(any_pcp_visits, mean, .before = 6, 
                                   .complete = FALSE),
         crac_pcp_roll7 = slide_dbl(crac_ratio_pcp, mean, .before = 6, 
                                   .complete = FALSE),
         cr_op_roll7   = slide_dbl(cr_op_visits,  mean,   .before = 6, 
                                   .complete = FALSE),
         any_op_roll7  = slide_dbl(any_op_visits, mean,   .before = 6, 
                                   .complete = FALSE),
         crac_op_roll7 = slide_dbl(crac_ratio_op, mean,   .before = 6, 
                                   .complete = FALSE)) |>
  ungroup()

# This establishes exposure rectangles
msa_exp_intervals <- elig_op_smoke2 |>
  filter(date >= as.Date("2015-07-01") & date <= as.Date("2015-09-30")) |>
  distinct(msa, date, smoke) |>
  arrange(msa, date) |>
  group_by(msa) |>
  mutate(
    run_id = cumsum(smoke != lag(smoke, default = 0))
  ) |>
  filter(smoke == 1) |>
  group_by(msa, run_id) |>
  summarise(start = min(date), end = max(date), .groups = "drop")


# ---- Prep to plot at State level ----
state_plotting <- elig_op_smoke2 |>
  filter(date >= as.Date("2015-07-01") & date <= as.Date("2015-09-30")) |>
  group_by(date) |>
  summarize(cr_ed_visits   = sum(cr_ed,       na.rm = TRUE),
            any_ed_visits  = sum(any_ed,      na.rm = TRUE),
            crac_ratio_ed  = sum(cr_ed,       na.rm = TRUE) / sum(any_ed,  na.rm = TRUE),
            cr_pcp_visits  = sum(cr_pcp,      na.rm = TRUE),
            any_pcp_visits = sum(any_pcp,     na.rm = TRUE),
            crac_ratio_pcp = sum(cr_pcp,      na.rm = TRUE) / sum(any_pcp, na.rm = TRUE),
            cr_op_visits   = sum(any_cr_op,   na.rm = TRUE),
            any_op_visits  = sum(any_op,      na.rm = TRUE),
            crac_ratio_op  = sum(any_cr_op,   na.rm = TRUE) / sum(any_op,  na.rm = TRUE),
            pop_exposed    = sum(msa_exposed, na.rm = TRUE),
            .groups = "drop") |>
  arrange(date) |>
  mutate(cr_ed_roll7   = slide_dbl(cr_ed_visits,  mean,   .before = 6, 
                                   .complete = FALSE),
         any_ed_roll7  = slide_dbl(any_ed_visits, mean,   .before = 6, 
                                   .complete = FALSE),
         crac_ed_roll7 = slide_dbl(crac_ratio_ed, mean,   .before = 6, 
                                   .complete = FALSE),
         cr_pcp_roll7   = slide_dbl(cr_pcp_visits,  mean, .before = 6, 
                                    .complete = FALSE),
         any_pcp_roll7  = slide_dbl(any_pcp_visits, mean, .before = 6, 
                                    .complete = FALSE),
         crac_pcp_roll7 = slide_dbl(crac_ratio_pcp, mean, .before = 6, 
                                    .complete = FALSE),
         cr_op_roll7   = slide_dbl(cr_op_visits,  mean,   .before = 6, 
                                   .complete = FALSE),
         any_op_roll7  = slide_dbl(any_op_visits, mean,   .before = 6, 
                                   .complete = FALSE),
         crac_op_roll7 = slide_dbl(crac_ratio_op, mean,   .before = 6, 
                                   .complete = FALSE),
         exposed_roll7 = slide_dbl(pop_exposed, mean,     .before = 6, 
                                   .complete = FALSE))

# this scales the secondary axes
ed_scale        <- max(state_plotting$any_ed_roll7,   na.rm = TRUE) /
                   max(state_plotting$exposed_roll7,  na.rm = TRUE)

ed_ratio_scale  <- max(state_plotting$crac_ed_roll7,  na.rm = TRUE) /
                   max(state_plotting$exposed_roll7,  na.rm = TRUE)

pcp_scale       <- max(state_plotting$any_pcp_roll7,  na.rm = TRUE) /
                   max(state_plotting$exposed_roll7,  na.rm = TRUE)

pcp_ratio_scale <- max(state_plotting$crac_pcp_roll7, na.rm = TRUE) /
                   max(state_plotting$exposed_roll7,  na.rm = TRUE)

op_scale        <- max(state_plotting$any_op_roll7,  na.rm = TRUE) /
                   max(state_plotting$exposed_roll7, na.rm = TRUE)

op_ratio_scale  <- max(state_plotting$crac_op_roll7, na.rm = TRUE) /
                   max(state_plotting$exposed_roll7, na.rm = TRUE)

# ---- Plot ED use ----
# This one plots all cause ED use vs CR ED use vs exposed pop by MSA
plotting |>
  ggplot(aes(x = date)) +
  geom_rect(
    data = msa_exp_intervals,
    aes(xmin = start - 1, xmax = end + 1, ymin = -Inf, ymax = Inf),
    fill = "orange",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = cr_ed_roll7, color = "Cardiorespiratory ED Visits")) +
  geom_line(aes(y = any_ed_roll7, color = "All ED Visits")) +
  scale_y_continuous(name = "ED Visits") +
  scale_color_manual(
    values = c("Cardiorespiratory ED Visits" = "firebrick",
               "All ED Visits"               = "steelblue")
  ) +
  facet_wrap(~ msa, scales = "free_y") +
  labs(x = "Date", 
       color = NULL,
       title = "ED visits vs. smoke exposure by MSA")

# This one plots the ratio of CR ED use to all cause ED use by MSA
plotting |>
  ggplot(aes(x = date)) +
  geom_rect(
    data = msa_exp_intervals,
    aes(xmin = start - 1, xmax = end + 1, ymin = -Inf, ymax = Inf),
    fill = "orange",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = crac_ed_roll7, color = "CR:AC Ratio")) +
  scale_y_continuous(name = "ED Visits") +
  scale_color_manual(values = c("CR:AC Ratio"   = "firebrick")) +
  facet_wrap(~ msa, scales = "free_y") +
  labs(x = "Date", 
       color = NULL,
       title = "ED visits vs. smoke exposure by MSA")

# This one plots all cause ED use vs CR ED use vs exposed pop for all of WA
state_plotting |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = cr_ed_roll7,
                color = "Cardiorespiratory ED Visits")) +
  geom_line(aes(y = any_ed_roll7,
                color = "All ED Visits")) +
  geom_line(aes(y = exposed_roll7 * ed_scale, 
                color = "Exposed population")) +
  scale_y_continuous(
    name = "ED Visits",
    sec.axis = sec_axis(~ . / ed_scale, name = "Exposed population")
    ) +
  scale_color_manual(
    values = c("Cardiorespiratory ED Visits" = "firebrick",
               "All ED Visits"               = "steelblue",
               "Exposed population"          = "orange")
  ) +
  labs(x = "Date", 
       color = NULL,
       title = "ED visits vs. smoke exposure Washington State")

# This one plots the ratio of CR ED use to all cause ED use for all of WA
state_plotting |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = crac_ed_roll7,
                color = "CR:AC Ratio")) +
  geom_line(aes(y = exposed_roll7 * ed_ratio_scale, 
                color = "Exposed population")) +
  scale_y_continuous(
    name = "CR:AC Ratio",
    sec.axis = sec_axis(~ . / ed_ratio_scale, name = "Exposed population")
  ) +
  scale_color_manual(
    values = c("CR:AC Ratio"        = "firebrick",
               "Exposed population" = "orange")
  ) +
  labs(x = "Date", 
       color = NULL,
       title = "ED visits vs. smoke exposure Washington State")

# ---- Plot PCP use ----
# This one plots all cause PCP use vs CR PCP use vs exposed pop by MSA
plotting |>
  ggplot(aes(x = date)) +
  geom_rect(
    data = msa_exp_intervals,
    aes(xmin = start - 1, xmax = end + 1, ymin = -Inf, ymax = Inf),
    fill = "orange",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = cr_pcp_roll7,  color = "Cardiorespiratory PCP Visits")) +
  geom_line(aes(y = any_pcp_roll7, color = "All PCP Visits")) +
  scale_y_continuous(name = "PCP Visits") +
  scale_color_manual(
    values = c("Cardiorespiratory PCP Visits" = "firebrick",
               "All PCP Visits"               = "steelblue")
  ) +
  facet_wrap(~ msa, scales = "free_y") +
  labs(x = "Date", 
       color = NULL,
       title = "PCP visits vs. smoke exposure by MSA")

# This one plots the ratio of CR PCP use to all cause PCP use by MSA
plotting |>
  ggplot(aes(x = date)) +
  geom_rect(
    data = msa_exp_intervals,
    aes(xmin = start - 1, xmax = end + 1, ymin = -Inf, ymax = Inf),
    fill = "orange",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = crac_pcp_roll7, color = "CR:AC Ratio")) +
  scale_y_continuous(name = "PCP Visits") +
  scale_color_manual(values = c("CR:AC Ratio"   = "firebrick")) +
  facet_wrap(~ msa, scales = "free_y") +
  labs(x = "Date", 
       color = NULL,
       title = "PCP visits vs. smoke exposure by MSA")

# This one plots all cause PCP use vs CR PCP use vs exposed pop for all of WA
state_plotting |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = cr_pcp_roll7,
                color = "Cardiorespiratory PCP Visits")) +
  geom_line(aes(y = any_pcp_roll7,
                color = "All PCP Visits")) +
  geom_line(aes(y = exposed_roll7 * pcp_scale, 
                color = "Exposed population")) +
  scale_y_continuous(
    name = "PCP Visits",
    sec.axis = sec_axis(~ . / pcp_scale, name = "Exposed population")
  ) +
  scale_color_manual(
    values = c("Cardiorespiratory PCP Visits" = "firebrick",
               "All PCP Visits"               = "steelblue",
               "Exposed population"           = "orange")
  ) +
  labs(x = "Date", 
       color = NULL,
       title = "PCP visits vs. smoke exposure Washington State")

# This one plots the ratio of CR PCP use to all cause PCP use for all of WA
state_plotting |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = crac_pcp_roll7,
                color = "CR:AC Ratio")) +
  geom_line(aes(y = exposed_roll7 * pcp_ratio_scale, 
                color = "Exposed population")) +
  scale_y_continuous(
    name = "CR:AC Ratio",
    sec.axis = sec_axis(~ . / ed_ratio_scale, name = "Exposed population")
  ) +
  scale_color_manual(
    values = c("CR:AC Ratio"        = "firebrick",
               "Exposed population" = "orange")
  ) +
  labs(x = "Date", 
       color = NULL,
       title = "PCP visits vs. smoke exposure Washington State")

# ---- Plot all outpatient use ----
# This one plots all cause OP use vs CR OP use vs exposed pop by MSA
plotting |>
  ggplot(aes(x = date)) +
  geom_rect(
    data = msa_exp_intervals,
    aes(xmin = start - 1, xmax = end + 1, ymin = -Inf, ymax = Inf),
    fill = "orange",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = cr_op_roll7, color  = "Cardiorespiratory OP Visits")) +
  geom_line(aes(y = any_op_roll7, color = "All OP Visits")) +
  scale_y_continuous(name = "OP Visits") +
  scale_color_manual(
    values = c("Cardiorespiratory OP Visits" = "firebrick",
               "All OP Visits"               = "steelblue")
  ) +
  facet_wrap(~ msa, scales = "free_y") +
  labs(x = "Date", 
       color = NULL,
       title = "OP visits vs. smoke exposure by MSA")

# This one plots the ratio of CR OP use to all cause OP use by MSA
plotting |>
  ggplot(aes(x = date)) +
  geom_rect(
    data = msa_exp_intervals,
    aes(xmin = start - 1, xmax = end + 1, ymin = -Inf, ymax = Inf),
    fill = "orange",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = crac_op_roll7, color = "CR:AC Ratio")) +
  scale_y_continuous(name = "OP Visits") +
  scale_color_manual(values = c("CR:AC Ratio"   = "firebrick")) +
  facet_wrap(~ msa, scales = "free_y") +
  labs(x = "Date", 
       color = NULL,
       title = "OP visits vs. smoke exposure by MSA")

# This one plots all cause OP use vs CR OP use vs exposed pop for all of WA
state_plotting |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = cr_op_roll7,
                color = "Cardiorespiratory OP Visits")) +
  geom_line(aes(y = any_op_roll7,
                color = "All OP Visits")) +
  geom_line(aes(y = exposed_roll7 * op_scale, 
                color = "Exposed population")) +
  scale_y_continuous(
    name = "OP Visits",
    sec.axis = sec_axis(~ . / ed_scale, name = "Exposed population")
  ) +
  scale_color_manual(
    values = c("Cardiorespiratory OP Visits" = "firebrick",
               "All OP Visits"               = "steelblue",
               "Exposed population"          = "orange")
  ) +
  labs(x = "Date", 
       color = NULL,
       title = "OP visits vs. smoke exposure Washington State")

# This one plots the ratio of CR OP use to all cause OP use for all of WA
state_plotting |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = crac_op_roll7,
                color = "CR:AC Ratio")) +
  geom_line(aes(y = exposed_roll7 * op_ratio_scale, 
                color = "Exposed population")) +
  scale_y_continuous(
    name = "CR:AC Ratio",
    sec.axis = sec_axis(~ . / op_ratio_scale, name = "Exposed population")
  ) +
  scale_color_manual(
    values = c("CR:AC Ratio"        = "firebrick",
               "Exposed population" = "orange")
  ) +
  labs(x = "Date", 
       color = NULL,
       title = "OP visits vs. smoke exposure Washington State")
