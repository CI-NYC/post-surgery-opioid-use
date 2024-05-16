# -------------------------------------
# Script: exclude_dems
# Author: Anton Hung 2024-05-15
# Purpose: Create exclusion flags for pregnancy, dual eligibility, and cancer diagnosis
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)
library(tictoc)

surgeries <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")
setDT(surgeries)

elig_nested <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdebse/elig_nested.rds")


preg_elig_cds <- c("05", "53", "64", "68")
cancer_elig_cds <- c("34") # breast cancer treatment
dual_elig_cds <- c("23","24","25","26") 


elig_long <-
  elig_nested |>
  right_join(surgeries) |>
  unnest(data) |>
  rename(year = RFRNC_YR) |>
  select(-ELGBLTY_GRP_CD_LTST) |>
  pivot_longer(cols = starts_with("ELGBLTY_GRP_CD"),
               names_to = "month",
               values_to = "elig_code",
               values_drop_na = T) |>
  mutate(month = parse_number(month),
         year = as.numeric(year),
         elig_dt = as.Date(paste0(year, "-", month, "-01")))

setDT(elig_long)
last_elig_cal <- elig_long[elig_dt %within% interval(washout_start_dt, discharge_dt %m+% days(14)), ][order(elig_dt)] |>
  group_by(BENE_ID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(BENE_ID, CLM_ID, washout_cal_elig_code = elig_code)

elig_clean <- last_elig_cal |>
  mutate(cohort_exclusion_pregnancy =  case_when(washout_cal_elig_code %in% preg_elig_cds ~ 1,
                                                     TRUE ~ 0),
         cohort_exclusion_cancer_elig = case_when(washout_cal_elig_code %in% cancer_elig_cds ~ 1,
                                                      TRUE ~ 0),
         cohort_exclusion_dual = case_when(washout_cal_elig_code %in% dual_elig_cds ~ 1,
                                               TRUE ~ 0)) |>
  select(CLM_ID, cohort_exclusion_pregnancy, cohort_exclusion_cancer_elig, cohort_exclusion_dual)

saveRDS(elig_clean, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_dems.rds")
