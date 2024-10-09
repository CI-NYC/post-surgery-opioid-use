# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(dplyr)
library(lubridate)
library(tictoc)

uncleaned_cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

months_to_dropout <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")
hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")
oud <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_oud.rds")
moud <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_moud.rds")


# Hillary
cohort <- uncleaned_cohort |>
  select(BENE_ID, PRCDR_CD, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt))
  # left_join(oud |> select(BENE_ID, oud_dt)) |>
  # left_join(moud |> select(BENE_ID, moud_start_dt))

setDT(cohort)

cohort$time_to_outcome <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$oud_hillary_dt) / months(1) 
  / 6)
cohort$time_to_censor <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$enrolled_until) / months(1) 
  / 6)

cohort <- cohort[, status := as.numeric(!is.na(time_to_outcome) & 
                   time_to_outcome < time_to_censor)]
cohort$time <- pmin(cohort$time_to_censor,cohort$time_to_outcome, na.rm=T)

cohort <- cohort |>
  select(BENE_ID, status,time)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/hillary_KM.rds")


# MOUD
cohort <- uncleaned_cohort |>
  select(BENE_ID, PRCDR_CD, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(moud |> select(BENE_ID, moud_start_dt))
# left_join(oud |> select(BENE_ID, oud_dt)) |>


setDT(cohort)

cohort$time_to_outcome <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$moud_start_dt) / months(1) 
  / 6)
cohort$time_to_censor <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$enrolled_until) / months(1) 
  / 6)

cohort <- cohort[, status := as.numeric(!is.na(time_to_outcome) & 
                                          time_to_outcome < time_to_censor)]
cohort$time <- pmin(cohort$time_to_censor,cohort$time_to_outcome, na.rm=T)

cohort <- cohort |>
  select(BENE_ID, status,time)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/moud_KM.rds")


# Comprehensive OUD
cohort <- uncleaned_cohort |>
  select(BENE_ID, PRCDR_CD, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(oud |> select(BENE_ID, oud_dt))

setDT(cohort)

cohort$time_to_outcome <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$oud_dt) / months(1) 
  / 6)
cohort$time_to_censor <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$enrolled_until) / months(1) 
  / 6)

cohort <- cohort[, status := as.numeric(!is.na(time_to_outcome) & 
                                          time_to_outcome < time_to_censor)]
cohort$time <- pmin(cohort$time_to_censor,cohort$time_to_outcome, na.rm=T)

cohort <- cohort |>
  select(BENE_ID, status,time)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/oud_KM.rds")