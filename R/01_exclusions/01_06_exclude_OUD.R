# -------------------------------------
# Script: exclude_oud
# Author: Anton Hung
# Purpose: exclude beneficiaries on the basis of having pre-existing OUD. Defined as
#           OUD within the 6-month pre-operative period using the hillary method
# Notes:
# -------------------------------------
library(data.table)
library(lubridate)
library(dplyr)

# load all the surgeries
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims_with_opioids.rds")
setDT(claims)
# cohort <- cohort[,.(BENE_ID, oud_poison_dt)]

oud_hillary <- readRDS("/mnt/general-data/disability/create_cohort/final/oud_hillary.rds")
setDT(oud_hillary)
oud_hillary <- oud_hillary[, .(BENE_ID, oud_hillary_dt)]

# join oud to overdose dates
claims <- merge(claims, oud_hillary, by="BENE_ID", all.x=T)

# exclude OD
claims[, cohort_exclusion_oud_hillary := case_when(oud_hillary_dt %within% interval(LINE_SRVC_BGN_DT %m-% months(6), LINE_SRVC_BGN_DT) ~ 1, TRUE ~ 0)]

cohort_exclusion_oud_hillary <- claims[, .(CLM_ID, cohort_exclusion_oud_hillary)]

saveRDS(cohort_exclusion_oud_hillary, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/cohort_exclusion_hillary.rds")
