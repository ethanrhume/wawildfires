# =============================================================================
# Script:  06_establish_population.R
# Author:  Ethan Hume
# Date:    2026-06-10
# Inputs:  1. Legacy Merative commercial and Medicare outpatient, inpatient,
#             and eligibility CSVs for 2014-2015
# Purpose: Establishes the study-eligible population for 2014-2015: identifies
#          enrollees with a qualifying asthma/COPD diagnosis (per CMS CCW
#          codes) in outpatient or inpatient claims, identifies enrollees
#          with continuous eligibility across the study period, and combines
#          both criteria with WA residence and MSA membership.
# Outputs: 1. Qualifying outpatient/inpatient diagnosis tables
#             (data/op_reference_1415.fst, data/ip_reference_1415.fst).
#          2. Eligibility tables (data/elig_table.rds,
#             data/elig_period_1415.rds).
#          3. Final eligible population (data/elig_wa.fst).
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
               PRROC) #Loads necessary packages + installs if needed

here::i_am("scripts/09_establish_population.R") #Ensures `here` properly IDs top-level directory
set.seed(839)

# ---- Read in outpatient data ----
op_comm_14_path <- "E:/Temp/ccaeo141.csv"
op_comm_15_path <- "E:/Temp/ccaeo151.csv"
op_med_14_path  <- "E:/Temp/mdcro141.csv"
op_med_15_path  <- "E:/Temp/mdcro151.csv"

op_comm_14 <- fread(op_comm_14_path)
op_comm_15 <- fread(op_comm_15_path)
op_med_14  <- fread(op_med_14_path)
op_med_15  <- fread(op_med_15_path)

# ---- Identify outpatient claims for asthma & COPD that qualify an enrollee  ----
# First, not all versions of PROCTYP are the same data class. Coerce to character
op_dfs <-list(op_comm_14,  op_med_14, op_comm_15, op_med_15)
op_dfs <- lapply(op_dfs, function(df) {
  df |> mutate(PROCTYP = as.character(PROCTYP))
})

# Combine into one df for manipulation
op_1415 <- rbindlist(op_dfs, fill = TRUE, use.names = TRUE)
rm(op_dfs, op_comm_14, op_comm_15, op_med_14, op_med_15)

#' Outpatient valid codes are per CMS Chronic Conditions Warehouse updated 3/23
#' for Asthma or COPD
icd9_asthma_codes  <- c("49300", "49301", "49302", "49310", "49311", "49312", 
                        "49320", "49321", "49322", "49381", "49382", "49390", 
                        "49391", "49392")

icd9_copd_codes    <- c("490", "4910", "4911", "49120", "49121", "49122", 
                        "4918", "4919", "4920", "4928", "4940", "4941", 
                        "496")

icd9_resp_codes <- c(icd9_asthma_codes, icd9_copd_codes)

icd10_asthma_codes <- c("J4520", "J4521", "J4522", "J4530", "J4531", 
                        "J4532", "J4540", "J4541", "J4542", "J4550", 
                        "J4551", "J4552", "J45901", "J45902", "J45909", 
                        "J45990", "J45991", "J45998", "J8283")

icd10_copd_codes   <- c("J40", "J410", "J411", "J418", "J42", 
                        "J430", "J431", "J432", "J438", "J439", 
                        "J440", "J441", "J449", "J470", "J471", 
                        "J479")

icd10_resp_codes <- c(icd10_asthma_codes, icd10_copd_codes)

#' Here we keep only those who have at least one asthma or COPD outpatient 
#' claim in the reference year. 

#' Reference period for chronic conditions is the entire year prior to the 
#' fire season being examined, and the first six months of the year being examined.

op_reference_1415 <- op_1415 |>
  filter(EGEOLOC == 65) |>
  mutate(SVCDATE = as.Date(SVCDATE, format = "%m/%d/%Y")) |>
  filter(
    SVCDATE >= as.Date("2014-01-01") & SVCDATE <= as.Date("2015-06-30"),
    (DXVER == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_resp_codes)) |
    (DXVER == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_resp_codes))
  ) |>
  mutate(
    asthma = case_when(
      DXVER == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_asthma_codes) ~ 1,
      DXVER == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_asthma_codes) ~ 1,
      TRUE ~ 0
    ),
    copd = case_when(
      DXVER == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_copd_codes) ~ 1,
      DXVER == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_copd_codes) ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(ENROLID) |>
  summarize(n_asthma = sum(asthma), n_copd = sum(copd)) |>
  ungroup() |>
  filter(
    n_asthma >= 1 | n_copd >= 1
  )

op_qualifiers <- unique(op_reference_1415$ENROLID)

write_fst(op_reference_1415, here("data", "op_reference_1415.fst"))
rm(op_reference_1415, op_1415)

# ---- Read in inpatient data ----
ip_comm_14_path <- "E:/Temp/ccaei141.csv"
ip_comm_15_path <- "E:/Temp/ccaei151.csv"
ip_med_14_path  <- "E:/Temp/mdcri141.csv"
ip_med_15_path  <- "E:/Temp/mdcri151.csv"

ip_comm_14 <- fread(ip_comm_14_path) 
ip_comm_15 <- fread(ip_comm_15_path)
ip_med_14  <- fread(ip_med_14_path)
ip_med_15  <- fread(ip_med_15_path)

# ---- Identify inpatient claims for asthma & COPD that qualify an enrollee  ----
# Combine into one df for manipulation
ip_dfs <-list(ip_comm_14,  ip_med_14, ip_comm_15, ip_med_15)
ip_1415 <- rbindlist(ip_dfs, fill = TRUE, use.names = TRUE)
rm(ip_dfs, ip_comm_14, ip_comm_15, ip_med_14, ip_med_15)

#' Here we keep only those who have at least two asthma or two COPD inpatient 
#' claims in the reference year. This will be combined with outpatient qualifiers 
#' then used as a key to filter eligibility.

#' Reference period for chronic conditions is one year prior to start of study.

ip_reference_1415 <- ip_1415 |>
  filter(EGEOLOC == 65) |>
  mutate(ADMDATE = as.Date(ADMDATE, format = "%m/%d/%Y")) |>
  filter(
    ADMDATE >= as.Date("2014-01-01") & ADMDATE <= as.Date("2015-06-30"),
    (DXVER == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_resp_codes)) |
    (DXVER == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_resp_codes))
  ) |>
  mutate(
    asthma = case_when(
      DXVER == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_asthma_codes) ~ 1,
      DXVER == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_asthma_codes) ~ 1,
      TRUE ~ 0
    ),
    copd = case_when(
      DXVER == 9 & if_any(starts_with("DX"), ~ .x %in% icd9_copd_codes) ~ 1,
      DXVER == 0 & if_any(starts_with("DX"), ~ .x %in% icd10_copd_codes) ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(ENROLID) |>
  summarize(n_asthma = sum(asthma), n_copd = sum(copd)) |>
  ungroup() |>
  filter(
    n_asthma >= 1 | n_copd >= 1
  )

ip_qualifiers <- unique(ip_reference_1415$ENROLID)

write_fst(ip_reference_1415, here("data", "ip_reference_1415.fst"))
rm(ip_reference_1415, ip_1415)

# ---- Read in eligibility files ----
elig_comm_14_path <- "E:/Temp/ccaet141.csv"
elig_comm_15_path <- "E:/Temp/ccaet151.csv"
elig_med_14_path  <- "E:/Temp/mdcrt141.csv"
elig_med_15_path  <- "E:/Temp/mdcrt151.csv"

elig_comm_14 <- fread(elig_comm_14_path)
elig_comm_15 <- fread(elig_comm_15_path)
elig_med_14  <- fread(elig_med_14_path)
elig_med_15  <- fread(elig_med_15_path)

# ---- Identify enrollees eligible by length of time enrolled ----
elig_dfs <-list(elig_comm_14,  elig_med_14, elig_comm_15, elig_med_15)

elig_1415 <- rbindlist(elig_dfs, fill = TRUE, use.names = TRUE)
rm(elig_dfs, elig_comm_14,  elig_med_14, elig_comm_15, elig_med_15)

elig_period_1415 <- elig_1415 |>
  filter(EGEOLOC == 65) |>
  mutate(
    DTSTART = as.Date(DTSTART, format = "%m/%d/%Y"),
    DTEND   = as.Date(DTEND, format = "%m/%d/%Y")
  ) |>
  filter(
    DTSTART <= as.Date("2015-09-30"),
    DTEND   >= as.Date("2015-01-01")
  ) |>
  group_by(ENROLID) |>
  arrange(DTSTART, .by_group = TRUE) |>
  mutate(
    gap = DTSTART > lag(DTEND) + days(1)
  ) |>
  summarize(
    covers_start = min(DTSTART) <= as.Date("2015-01-01"),
    covers_end   = max(DTEND)   >= as.Date("2015-09-30"),
    has_gap      = any(gap, na.rm = TRUE)
  ) |>
  filter(covers_start & covers_end & !has_gap)

elig_qualifiers <- unique(elig_period_1415$ENROLID)

elig_table <- elig_1415 |>
  filter(ENROLID %in% elig_qualifiers)

saveRDS(elig_table, here("data", "elig_table.rds"))
saveRDS(elig_period_1415, here("data", "elig_period_1415.rds"))
rm(elig_1415)

# ---- Identify enrollees who meet all inclusion criteria ----
elig_wa <- elig_period_1415 |>
  filter(
    ENROLID %in% op_qualifiers | ENROLID %in% ip_qualifiers,
    ENROLID %in% elig_qualifiers
  )

elig_wa_test6 <- elig_op_smoke2 |>
  filter(
    enrolid %in% op_qualifiers | enrolid %in% ip_qualifiers,
    enrolid %in% elig_qualifiers
  ) |>
  filter(
    (!is.na(msa) & msa != 0 & msa != "0")
  )
  
write_fst(elig_wa, here("data", "elig_wa.fst"))
rm(elig_period_1415)
