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

# met <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_met.rds") |>
#   rename(met_start_dt = moud_start_dt)
# nal <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_nal.rds") |>
#   rename(nal_start_dt = moud_start_dt)
# bup <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_bup.rds") |>
#   rename(bup_start_dt = moud_start_dt)
poison <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_poison.rds")
hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")
oud <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_oud.rds")
moud <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_moud.rds")

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
  select(BENE_ID, PRCDR_CD, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(poison |> select(BENE_ID, oud_poison_dt)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt)) |>
  left_join(oud |> select(BENE_ID, oud_dt)) |>
  left_join(moud |> select(BENE_ID, moud_start_dt))

# for (month in 1:24){
#   print(paste("Processing month:", month))
#   tic()
#   censor_column <- paste0("C_", month)
#   OD_column <- paste0("Y1_", month)
#   OUD_column <- paste0("Y2_", month)
#   met_column <- paste0("Y3_", month)
#   nal_column <- paste0("Y4_", month)
#   bup_column <- paste0("Y5_", month)
# 
#   cohort <- cohort |>
#     mutate(
#       {{censor_column}} := ifelse(enrolled_until <= followup_start_dt %m+% months(month), 1, 0),
#       {{OD_column}} := ifelse(!is.na(oud_poison_dt) & oud_poison_dt <= followup_start_dt %m+% months(month), 1, 0),
#       {{OUD_column}} := ifelse(!is.na(oud_hillary_dt) & oud_hillary_dt <= followup_start_dt %m+% months(month), 1, 0),
#       {{met_column}} := ifelse(!is.na(met_start_dt) & met_start_dt <= followup_start_dt %m+% months(month), 1, 0),
#       {{nal_column}} := ifelse(!is.na(nal_start_dt) & nal_start_dt <= followup_start_dt %m+% months(month), 1, 0),
#       {{bup_column}} := ifelse(!is.na(bup_start_dt) & bup_start_dt <= followup_start_dt %m+% months(month), 1, 0),
#     )
#   toc()
# }
# 
# 
# saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/outcomes_wide.rds")

for (month in 1:4){
  print(paste("Processing month:", month))
  tic()
  censor_column <- paste0("C_", month)
  # poison_column <- paste0("Y1_", month)
  hillary_column <- paste0("Y2_", month)
  oud_column <- paste0("Y3_", month)
  moud_column <- paste0("Y4_", month)

  cohort <- cohort |>
    mutate(
      {{censor_column}} := ifelse(enrolled_until <= followup_start_dt %m+% months(month*6), 0, 1),
      # {{poison_column}} := ifelse(!is.na(oud_poison_dt) & oud_poison_dt <= followup_start_dt %m+% months(month*6), 1, 0),
      {{hillary_column}} := ifelse(!is.na(oud_hillary_dt) & oud_hillary_dt <= followup_start_dt %m+% months(month*6), 1, 0),
      {{oud_column}} := ifelse(!is.na(oud_dt) & oud_dt <= followup_start_dt %m+% months(month*6), 1, 0),
      {{moud_column}} := ifelse(!is.na(moud_start_dt) & moud_start_dt <= followup_start_dt %m+% months(month*6), 1, 0),
    )
  toc()
}


saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/outcomes_wide_6mos.rds")


####### UNCOMMENT BOTTOM PORTION OF NEEDED
# cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/outcomes_wide_6mos.rds")


# Carrying forward last value if someone has been censored.
# Previously, there was a problem where some small number of people would be re-enrolled, and therefore, their outcome variables might change, even though they had been censored earlier.
# This should not be allowed, so I changed it, for those who were censored, to overwrite all later values with their last uncensored value.


# for (i in 1:4){
#   print(paste("Processing month:", month))
# 
#   tic()
#   censor_column <- paste0("C_", i)
#   last_uncensored <- c(paste0("Y1_", i),
#                     paste0("Y2_", i),
#                     paste0("Y3_", i),
#                     paste0("Y4_", i))
# 
#   censored <- which(cohort[,censor_column] == 0)
# 
#   for (j in i:4){
#     carry_forward_columns <- c(paste0("Y1_", j),
#                                paste0("Y2_", j),
#                                paste0("Y3_", j),
#                                paste0("Y4_", j))
#     cohort[censored, carry_forward_columns] <- cohort[censored, last_uncensored]
# 
#   }
# 
#   toc()
# }
# 
# saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/outcomes_wide_6mos.rds")
