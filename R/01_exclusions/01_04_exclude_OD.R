# -------------------------------------
# Script: exclude OD
# Author: Anton Hung
# Purpose: creating exclusion variable for beneficiaries who have a history of OD during the 6 months continuous enrollment period prior to surgery
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)

# load all the people
cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds")
setDT(cohort)
cohort <- cohort[,.(BENE_ID, oud_poison_dt)]

# load all the surgeries
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims_with_opioids.rds")
setDT(claims)

# join surgery dates to overdose dates
claims <- merge(claims, cohort, by="BENE_ID", all.x=T)

# exclude OD
claims[, cohort_exclusion_poison := case_when(oud_poison_dt %within% interval(LINE_SRVC_BGN_DT %m-% months(6), LINE_SRVC_BGN_DT) ~ 1, TRUE ~ 0)]

cohort_exclusion_poison <- claims[, .(CLM_ID, cohort_exclusion_poison)]

saveRDS(cohort_exclusion_poison, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/cohort_exclusion_poison.rds")
