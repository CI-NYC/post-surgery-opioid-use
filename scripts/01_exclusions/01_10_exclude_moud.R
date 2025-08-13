# -------------------------------------
# Script: exclude_moud
# Author: Anton Hung 2024-05-16
# Purpose: 
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)

# load all the surgeries
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")
setDT(claims)
# cohort <- cohort[,.(BENE_ID, oud_poison_dt)]

met <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_met.rds") |> as.data.table()
nal <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_nal.rds") |> as.data.table()
bup <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_bup.rds") |> as.data.table()


# join MET to overdose dates
claims_joined_moud <- left_join(claims, met, by="BENE_ID", relationship = "many-to-many")

# Using the logic that two date ranges overlap if: (StartDate1 <= EndDate2) and (StartDate2 <= EndDate1)
# Identifying met claims within the washout period
# claims_joined_moud[, has_met := as.numeric(moud_start_dt <= discharge_dt %m+% days(14) &
#                                  washout_start_dt <= moud_end_dt)] 
claims_joined_moud[, has_met := as.numeric(moud_start_dt <= discharge_dt &
                                             washout_start_dt <= moud_end_dt)] 

cohort_exclusion_moud_met <- claims_joined_moud |>
  mutate(has_met = case_when(is.na(has_met) ~ 0, TRUE ~ has_met)) |>
  group_by(CLM_ID) |>
  mutate(cohort_exclusion_moud_met = as.numeric(any(has_met == 1))) |>
  ungroup() |>
  select(BENE_ID, CLM_ID, cohort_exclusion_moud_met) |>
  distinct()

# join nal to overdose dates
claims_joined_moud <- left_join(claims, nal, by="BENE_ID", relationship = "many-to-many")

# Identifying nal claims within the washout period
claims_joined_moud[, has_nal := as.numeric(moud_start_dt <= discharge_dt %m+% days(14) &
                                 washout_start_dt <= moud_end_dt)] 

cohort_exclusion_moud_nal <- claims_joined_moud |>
  mutate(has_nal = case_when(is.na(has_nal) ~ 0, TRUE ~ has_nal)) |>
  group_by(CLM_ID) |>
  mutate(cohort_exclusion_moud_nal = as.numeric(any(has_nal == 1))) |>
  ungroup() |>
  select(CLM_ID, cohort_exclusion_moud_nal) |>
  distinct()

# join bup to overdose dates
claims_joined_moud <- left_join(claims, bup, by="BENE_ID", relationship = "many-to-many")

# Identifying bup claims within the washout period
claims_joined_moud[, has_bup := as.numeric(moud_start_dt <= discharge_dt %m+% days(14) &
                                 washout_start_dt <= moud_end_dt)] 

cohort_exclusion_moud_bup <- claims_joined_moud |>
  mutate(has_bup = case_when(is.na(has_bup) ~ 0, TRUE ~ has_bup)) |>
  group_by(CLM_ID) |>
  mutate(cohort_exclusion_moud_bup = as.numeric(any(has_bup == 1))) |>
  ungroup() |>
  select(CLM_ID, cohort_exclusion_moud_bup) |>
  distinct()


cohort_exclusion_moud <- cohort_exclusion_moud_met |>
  left_join(cohort_exclusion_moud_nal, by = "CLM_ID") |>
  left_join(cohort_exclusion_moud_bup, by = "CLM_ID")

saveRDS(cohort_exclusion_moud, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_moud.rds")
