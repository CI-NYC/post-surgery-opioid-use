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

oud_hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_hillary_dts.rds")
setDT(oud_hillary)
oud_hillary <- oud_hillary[, .(BENE_ID, oud_hillary_dt)]

# join oud to overdose dates
claims <- left_join(claims, oud_hillary, by="BENE_ID")

# exclude OD
# cohort_exclusion_oud_hillary <- claims[, .(cohort_exclusion_oud_hillary =
#                                                                as.numeric(any(oud_hillary_dt %within% 
#                                                                                 interval(surgery_dt %m-% months(6), surgery_dt %m-% months(1))))), 
#                                                          by = .(BENE_ID, CLM_ID)]


claims[, has_hillary := as.numeric(oud_hillary_dt %within% interval(surgery_dt %m-% months(6), surgery_dt))] 

cohort_exclusion_oud_hillary <- claims |>
  mutate(has_hillary = case_when(is.na(has_hillary) ~ 0, TRUE ~ has_hillary)) |>
  group_by(CLM_ID) |>
  mutate(cohort_exclusion_oud_hillary = as.numeric(any(has_hillary == 1))) |>
  ungroup() |>
  select(BENE_ID, CLM_ID, cohort_exclusion_oud_hillary) |>
  distinct()


# claims[, cohort_exclusion_oud_hillary := case_when(oud_hillary_dt %within% interval(LINE_SRVC_BGN_DT %m-% months(6), LINE_SRVC_BGN_DT) ~ 1, TRUE ~ 0)]

# claims$cohort_exclusion_oud_hillary <- sapply(1:nrow(claims), function(i){
#   hillary_dts <- oud_hillary[BENE_ID == claims[i, BENE_ID], "oud_hillary_dt"]
#   
#   return (as.numeric(any(hillary_dts$oud_hillary_dt %within% interval(claims[i, surgery_dt] %m-% months(6),
#                                                          claims[i, surgery_dt]))))
# })
# 
# cohort_exclusion_oud_hillary <- claims[, .(CLM_ID, cohort_exclusion_oud_hillary)]

saveRDS(cohort_exclusion_oud_hillary, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_hillary.rds")
