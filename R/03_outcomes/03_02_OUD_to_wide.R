# -------------------------------------
# Script: OUD_to_wide
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(tictoc)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

months_to_dropout <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")

met <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_met.rds") |>
  rename(met_start_dt = moud_start_dt)
nal <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_nal.rds") |>
  rename(nal_start_dt = moud_start_dt)
bup <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_bup.rds") |>
  rename(bup_start_dt = moud_start_dt)
poison <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_poison.rds")
hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")

# custom_round <- function(x) {
#   # Check if the fractional part of the number is at least approx 1 week (0.23 months)
#   if (x - floor(x) >= 0.23) {
#     return(ceiling(x))
#   } else {
#     return(floor(x)) 
#   }
# }
# cohort$months_to_dropout <- sapply(cohort$months_to_dropout, custom_round)

# hillary_dat <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds") |>
#   select(BENE_ID, oud_hillary_dt) |>
#   # filter(!is.na(oud_hillary_dt)) |>
#   left_join(surgery_dt[, c("BENE_ID", "surgery_dt")]) |>
#   left_join(cohort[, c("BENE_ID", "discharge_dt", "enrolled_until")])
# 
# for (month in 1:24){
#   censor_column <- paste0("C_", month)
#   outcome_column <- paste0("Y_", month)
#   hillary_dat <- hillary_dat |>
#     mutate({{censor_column}} := case_when(enrolled_until <= discharge_dt %m+% months(month) ~ 1, TRUE ~ 0),
#            {{outcome_column}} := case_when(oud_hillary_dt <= discharge_dt %m+% months(month) ~ 1, TRUE ~ 0))
# }
# 
# saveRDS(hillary_dat, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/hillary_wide.rds")
# 
# 
# 
# poison_dat <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_poison.rds") |>
#   select(BENE_ID, oud_poison_dt) |>
#   filter(!is.na(oud_poison_dt)) |>
#   left_join(surgery_dt[, c("BENE_ID", "surgery_dt")]) |>
#   left_join(cohort[, c("BENE_ID", "discharge_dt", "enrolled_until")])
# 
# for (month in 0:24){
#   censor_column <- paste0("C_", month)
#   outcome_column <- paste0("Y_", month)
#   poison_dat <- poison_dat |>
#     mutate({{censor_column}} := case_when(enrolled_until <= discharge_dt %m+% months(month) ~ 1, TRUE ~ 0),
#            {{outcome_column}} := case_when(oud_poison_dt <= discharge_dt %m+% months(month) ~ 1, TRUE ~ 0))
# }
# 
# saveRDS(poison_dat, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/poison_wide.rds")


cohort <- cohort |>
  select(BENE_ID, LINE_PRCDR_CD, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(met |> select(BENE_ID, met_start_dt)) |>
  left_join(nal |> select(BENE_ID, nal_start_dt)) |>
  left_join(bup |> select(BENE_ID, bup_start_dt)) |>
  left_join(poison |> select(BENE_ID, oud_poison_dt)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt))

for (month in 1:12){
  print(paste("Processing month:", month))
  tic()
  censor_column <- paste0("C_", month)
  OD_column <- paste0("Y1_", month)
  OUD_column <- paste0("Y2_", month)
  met_column <- paste0("Y3_", month)
  nal_column <- paste0("Y4_", month)
  bup_column <- paste0("Y5_", month)
  
  cohort <- cohort |>
    mutate({{censor_column}} := case_when(enrolled_until <= followup_start_dt %m+% months(month) ~ 1, TRUE ~ 0),
           {{OD_column}} := case_when(oud_poison_dt <= followup_start_dt %m+% months(month) ~ 1, TRUE ~ 0),
           {{OUD_column}} := case_when(oud_hillary_dt <= followup_start_dt %m+% months(month) ~ 1, TRUE ~ 0),
           {{met_column}} := case_when(met_start_dt <= followup_start_dt %m+% months(month) ~ 1, TRUE ~ 0),
           {{nal_column}} := case_when(nal_start_dt <= followup_start_dt %m+% months(month) ~ 1, TRUE ~ 0),
           {{bup_column}} := case_when(bup_start_dt <= followup_start_dt %m+% months(month) ~ 1, TRUE ~ 0),
           )
}


saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/outcomes_wide.rds")
