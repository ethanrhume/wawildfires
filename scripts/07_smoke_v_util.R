# =============================================================================
# Script:  07_smoke_v_util.R
# Author:  Ethan Hume
# Date:    2026-06-10
# Inputs:  1. Eligible population table (data/elig_table.rds) from
#             09_establish_population.R.
#          2. Legacy Merative 2015 outpatient claims CSVs (E:/Temp/).
#          3. Constructed MSA-day smoke indicators
#             (data/msa_smokedays_constructed.rds) and ZCTA-MSA-monitor
#             crosswalk (data/zcta_msa_monitor_clean.rds) from
#             04_transform_smoke.R.
# Purpose: Builds an enrollee-day-level dataset for 2015, flags outpatient
#          encounters as ED/PCP/other and cardiorespiratory-specific, merges
#          in MSA-level smoke exposure, and derives population-weighted
#          exposure metrics (MSA-level and statewide exposed population).
# Outputs: 1. Enrollee-day dataset with utilization and exposure variables
#             (data/data2015.fst).
#          2. MSA-day exposure table (data/exposure_msa_day.csv).
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

here::i_am("scripts/10_smoke_v_util.R") #Ensures `here` properly IDs top-level directory
set.seed(839)

# ---- Read in data ----
elig_table      <- readRDS(here("data", "elig_table.rds"))

op_comm_15_path <- "E:/Temp/ccaeo151.csv"
op_med_15_path  <- "E:/Temp/mdcro151.csv"
op_comm_15      <- fread(op_comm_15_path)
op_med_15       <- fread(op_med_15_path)
op_dfs          <-list(op_comm_15, op_med_15)
op_dfs          <- lapply(op_dfs, function(df) {
  df |> mutate(PROCTYP = as.character(PROCTYP))
})
outpatient <- rbindlist(op_dfs, fill = TRUE, use.names = TRUE)
rm(op_dfs, op_comm_15, op_med_15)

msa_smokedays_constructed <- readRDS(here("data", "msa_smokedays_constructed.rds"))
zcta_msa_monitor_clean <- readRDS(here("data", "zcta_msa_monitor_clean.rds"))

# ---- Construct table of enrollees, characteristics, and events ----
elig_table_full <- elig_table |> 
  mutate(
    DTSTART = as.Date(DTSTART, format = "%m/%d/%Y"),
    DTEND   = as.Date(DTEND, format = "%m/%d/%Y"),
    ENROLID = as.character(ENROLID)
    ) |>
  filter(
    DTSTART >= as.Date("2015-01-01"),
    MSA     != 0
    ) |>
  rowwise() |>
  mutate(
    date = list(seq(DTSTART, DTEND, by = "day"))
  ) |>
  mutate(
    MSA = case_when(
    MSA %in% c(42644, 45104) ~ "seattle_tacoma_bellevue",
    MSA == 38900             ~ "portland_vancouver_hillsboro",
    MSA == 48300             ~ "wenatchee_east_wenatchee",
    MSA == 14740             ~ "bremerton_silverdale_port_orchard",
    MSA == 13380             ~ "bellingham",
    MSA == 36500             ~ "olympia_lacey_tumwater",
    MSA == 34580             ~ "mount_vernon_anacortes",
    MSA == 47460             ~ "walla_walla",
    MSA == 28420             ~ "kennewick_richland",
    MSA == 31020             ~ "longview_kelso",
    MSA == 44060             ~ "spokane_spokane_valley",
    MSA == 49420             ~ "yakima",
    MSA == 30300             ~ "lewiston"
  )) |>
  unnest(date) |>
  ungroup() |>
  janitor::clean_names()

# ---- Prepare outpatient claims for merging with enrollee-days ----
ed_revcodes <- c(seq(0450, 0459), 0981)
ed_procs    <- c(seq(99281, 99285))
pc_revcodes <- c(0510, 0515, 0517, 0519, 0521, 0522)
pc_stdplacs <- c(11, 50, 72)
pc_procs    <- c(seq(99202, 99215), seq(99381, 99397))
serv_cats   <- c(10120, 10220, 10320, 10420, 10520, 12220, 20120, 20220, 21120, 
                 21220, 22120, 22320, 30120, 30220, 30320, 30420, 30520, 30620,
                 31120, 31220, 31320, 31420, 31520, 31620)

outpatient2 <- outpatient |>
  filter(ENROLID %in% elig_table_full$enrolid) |>
  mutate(
    any_ed    = case_when(
      STDPLAC == 23 | 
      SVCSCAT %in% serv_cats | 
      REVCODE %in% ed_revcodes |
      (PROC1  %in% ed_procs & PROCTYP == 1) ~ 1,
      TRUE                                  ~ 0),
    any_op    = 1,
    any_pcp   = case_when(
      STDPLAC %in% pc_stdplacs |
      REVCODE %in% pc_revcodes |
      (PROC1  %in% pc_procs & PROCTYP == 1) ~ 1,
      TRUE                                  ~ 0
    )) |> 
  mutate(
    cr_ed     = case_when(
      any_ed  == 1 & 
      (MDC    %in% c(04, 05) |
      (DXVER  == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_resp_codes)) |
      (DXVER  == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_resp_codes)))~ 1,
      TRUE                                                                 ~ 0),
    any_cr_op = case_when(
      (MDC     %in% c(04, 05) |
      (DXVER  == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_resp_codes)) |
      (DXVER  == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_resp_codes))) ~ 1,
      TRUE                                                                  ~ 0),
    cr_pcp    = case_when(
      any_pcp == 1 &
      (MDC     %in% c(04, 05) |
      (DXVER  == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_resp_codes)) |
      (DXVER  == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_resp_codes))) ~ 1,
      TRUE                                                                  ~ 0)
  ) |>
  rename(date = SVCDATE) |>
  janitor::clean_names() |>
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         enrolid = as.character(enrolid))

# ---- Merge outpatient claims and enrollee-days ----
elig_and_op <- elig_table_full |>
  left_join(
    outpatient2 |> 
      select(dx1, dx2, dx3, dx4, dxver, proc1, proctyp, enrolid, revcode, date, 
             cap_svc, cob, coins, copay, deduct, fachdid, facprof, netpay, 
             ntwkprov, paidntwk, pay, pddate, procgrp, procmod, provid, qty, 
             svcscat, tsvcdat, mdc, stdplac, stdprov, eidflag, enrflag, msclmid, 
             npi, units, any_op, any_ed, any_pcp, cr_ed, any_cr_op, cr_pcp), 
    by = c("enrolid", "date")) |>
  mutate(across(c(any_op, any_ed, any_pcp, cr_ed, any_cr_op, cr_pcp), 
                ~ replace_na(.x, 0)))
  
# ---- Prepare then merge exposed smoke table with enrollee-days ----
msa_smokedays_constructed2 <- msa_smokedays_constructed |>
  filter(year == 2015) |>
  select(msa, date, active_fire, smoke, msa_con)

elig_op_smoke <- left_join(elig_and_op, msa_smokedays_constructed2, 
                           by = c("msa", "date"))

# ---- Add variables for msa_pop, msa_exposed, wa_exposed ----
elig_op_smoke2 <- elig_op_smoke |>
  group_by(msa) |>
  mutate(
    msa_pop = n_distinct(enrolid)
  ) |>
  ungroup() |>
  group_by(msa, date) |>
  mutate(
    msa_exposed = case_when(
      smoke == 1 ~ msa_pop,
      smoke == 0 ~ 0
    )) |>
  ungroup() |>
  group_by(date) |>
  mutate(
    wa_exposed = sum(unique(msa_exposed))
  )

write_fst(elig_op_smoke2, here("data", "data2015.fst"))











expo <- expo |>
  mutate(msa_code = case_when(
        msa == "seattle_tacoma_bellevue" ~ NA_real_,
        msa == "portland_vancouver_hillsboro" ~ 38900,
        msa == "wenatchee_east_wenatchee" ~ 48300,
        msa == "bremerton_silverdale_port_orchard" ~ 14740,
        msa == "bellingham" ~ 13380,
        msa == "olympia_lacey_tumwater" ~ 36500,
        msa == "mount_vernon_anacortes" ~ 34580,
        msa == "walla_walla" ~ 47460,
        msa == "kennewick_richland" ~ 28420,
        msa == "longview_kelso" ~ 31020,
        msa == "spokane_spokane_valley" ~ 44060,
        msa == "yakima" ~ 49420,
        msa == "lewiston" ~ 30300
      ))

seattle_42644 <- expo |>
  filter(msa == "seattle_tacoma_bellevue") |>
  mutate(msa_code = 42644)

seattle_45104 <- expo |>
  filter(msa == "seattle_tacoma_bellevue") |>
  mutate(msa_code = 45104)

expo_coded <- expo |>
  filter(msa != "seattle_tacoma_bellevue") |>
  bind_rows(seattle_42644, seattle_45104)

expo_coded <- expo_coded |>
  select(-msa) |>
  rename(msa = msa_code)

write_csv(expo_coded, here("data", "exposure_msa_day.csv"))
