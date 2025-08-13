# -------------------------------------
# Script: exclude OD
# Author: Anton Hung
# Purpose: creating exclusion variable for beneficiaries who have a history of OD during the 6 months continuous enrollment period prior to surgery
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)

# # load all the people
# cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds")
# setDT(cohort)
# cohort <- cohort[,.(BENE_ID, oud_poison_dt)]

# load the oud_poison data
oud_poison_dt <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_poison_dts.rds")
setDT(oud_poison_dt)

# load all the surgeries
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")
setDT(claims)

# join surgery dates to overdose dates
claims <- left_join(claims, oud_poison_dt, by="BENE_ID", relationship = "many-to-many")


# exclude OD
# cohort_exclusion_oud_poison <- claims[, .(cohort_exclusion_oud_poison =
#                                              as.numeric(any(oud_poison_dt %within% 
#                                                               interval(surgery_dt %m-% months(6), surgery_dt %m-% months(1))))), 
#                                        by = .(BENE_ID, CLM_ID)]


# finding beneficiaries who have an OUD poison dt during the washout period until the end of the perioperative period
# claims[, has_poison := 
#                     as.numeric(oud_poison_dt %within% interval(washout_start_dt, discharge_dt %m+% days(14)))] 
claims[, has_poison := 
         as.numeric(oud_poison_dt %within% interval(washout_start_dt, discharge_dt))]

cohort_exclusion_oud_poison <- claims |>
  mutate(has_poison = case_when(is.na(has_poison) ~ 0, TRUE ~ has_poison)) |>
  group_by(CLM_ID) |>
  mutate(cohort_exclusion_oud_poison = as.numeric(any(has_poison == 1))) |>
  ungroup() |>
  select(BENE_ID, CLM_ID, cohort_exclusion_oud_poison) |>
  distinct()

# claims$cohort_exclusion_poison <- sapply(1:nrow(claims), function(i){
#   poison_dts <- oud_poison_dt[BENE_ID == claims[i, BENE_ID], "oud_poison_dt"]
#   
#   return (as.numeric(any(poison_dts$oud_poison_dt %within% interval(claims[i, surgery_dt] %m-% months(6),
#                                                          claims[i, surgery_dt]))))
# })

# cohort_exclusion_poison <- claims[, .(CLM_ID, cohort_exclusion_poison)]

saveRDS(cohort_exclusion_oud_poison, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_poison.rds")
