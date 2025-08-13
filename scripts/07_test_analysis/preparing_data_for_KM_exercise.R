# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(dplyr)
library(survival)
library(data.table)
library(tictoc)
library(lubridate)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

months_to_dropout <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")

hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")


cohort <- cohort |>
  select(BENE_ID, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt))

# current: censored + outcome = censored

# censored + outcome = outcome
for (month in 1:4){
  print(paste("Processing month:", month))
  tic()
  censor_column <- paste0("C_", month)
  hillary_column <- paste0("Y2_", month)
  
  cohort <- cohort |>
    mutate(
      {{censor_column}} := ifelse(enrolled_until <= followup_start_dt %m+% months(month*6) &
                                    (is.na(oud_hillary_dt) | enrolled_until <= oud_hillary_dt), 0, 1),
      {{hillary_column}} := ifelse(!is.na(oud_hillary_dt) & oud_hillary_dt <= followup_start_dt %m+% months(month*6), 1, 0)
    )
  toc()
}

# cohort[as.logical(cohort$Y2_1), "C_1"] <- 1
# 
# # uncensoring people if they gained the outcome during the same period that they were censored
# for (i in 2:4){
#   print(paste("Processing month:", i))
#   
#   outcome_new <- cohort[,paste0("Y2_", i)] - cohort[,paste0("Y2_", i-1)]
#   
#   cohort[as.logical(outcome_new), paste0("C_", i)] = 1
# }
# 
saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/final/outcome_prioritized.rds")


# continuous
cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
months_to_dropout <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")

hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")


cohort <- cohort |>
  select(BENE_ID, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt))

setDT(cohort)
cohort <- cohort[, status := !is.na(oud_hillary_dt) & 
                              oud_hillary_dt <= enrolled_until]

cohort$status = as.numeric(cohort$status)

cohort$time <- ifelse(cohort$status==1,
                      # 1. Depending on whether outcome or censor comes first, return 
                      # the time difference (in months) between the start date and 
                      # outcome/censor date.
                      # 2. Also, divide the difference by 6 and round up (to find which 
                      # semi-annual period the date falls within)
                      ceiling(
                        interval(cohort$followup_start_dt-1,cohort$oud_hillary_dt) / months(1) 
                        / 6),
                      ceiling(
                        interval(cohort$followup_start_dt-1,cohort$enrolled_until) / months(1) 
                        / 6)
)
cohort <- cohort|>
  select(BENE_ID,status,time)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/final/outcome_prioritized.rds")


# Censoring prioritized
cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
months_to_dropout <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")

hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")


cohort <- cohort |>
  select(BENE_ID, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt))

setDT(cohort)

cohort$time_to_outcome <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$oud_hillary_dt) / months(1) 
  / 6)
cohort$time_to_censor <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$enrolled_until) / months(1) 
  / 6)

cohort <- cohort[, status := !is.na(time_to_outcome) & 
                   time_to_outcome < time_to_censor]

cohort$status = as.numeric(cohort$status)

cohort$time <- pmin(cohort$time_to_censor,cohort$time_to_outcome, na.rm=T)

cohort <- cohort |>
  select(BENE_ID, status,time)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/final/censor_prioritized.rds")
