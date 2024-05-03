# -------------------------------------
# Script: OUD_to_wide
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------
library(data.table)
library(dplyr)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")

# custom_round <- function(x) {
#   # Check if the fractional part of the number is at least approx 1 week (0.23 months)
#   if (x - floor(x) >= 0.23) {
#     return(ceiling(x))
#   } else {
#     return(floor(x))
#   }
# }
# cohort$months_to_dropout <- sapply(cohort$months_to_dropout, custom_round)

hillary_dat <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds") |>
  select(BENE_ID, oud_hillary_dt) |>
  filter(!is.na(oud_hillary_dt)) |>
  left_join(cohort[, c("BENE_ID", "discharge_dt", "enrolled_until")])

for (month in 1:24){
  censor_column <- paste0("C_", month)
  outcome_column <- paste0("Y_", month)
  hillary_dat <- hillary_dat |>
    mutate({{censor_column}} := case_when(enrolled_until <= discharge_dt %m+% months(month) ~ 1, TRUE ~ 0),
           {{outcome_column}} := case_when(oud_hillary_dt <= discharge_dt %m+% months(month) ~ 1, TRUE ~ 0))
}

saveRDS(hillary_dat, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/hillary_wide.rds")



poison_dat <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_poison.rds") |>
  select(BENE_ID, oud_poison_dt) |>
  filter(!is.na(oud_poison_dt)) |>
  left_join(cohort[, c("BENE_ID", "discharge_dt", "enrolled_until")])

for (month in 1:24){
  new_column <- paste0("Y_", month)
  poison_dat <- poison_dat |>
    mutate({{censor_column}} := case_when(enrolled_until <= discharge_dt %m+% months(month) ~ 1, TRUE ~ 0),
           {{new_column}} := case_when(oud_poison_dt <= discharge_dt %m+% months(month) ~ 1, TRUE ~ 0))
}

saveRDS(poison_dat, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/poison_wide.rds")
