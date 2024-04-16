# -------------------------------------
# Script: exclude_oud
# Author: Anton Hung
# Purpose: exclude beneficiaries on the basis of having pre-existing OUD. Defined as
#           OUD within the 6-month pre-operative period using the hillary method
# Notes:
# -------------------------------------

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds")
setDT(cohort)
# cohort <- cohort[,.(BENE_ID, oud_poison_dt)]

oud_hillary <- readRDS("projects/create_cohort/data/final/oud_hillary.rds")
setDT(claims)

# join oud to overdose dates
cohort <- merge(cohort, oud_hillary, by="BENE_ID", all.x=T)
claims <- unique(claims)

# exclude OD
claims[, exclude_OD := case_when(oud_poison_dt %within% interval(LINE_SRVC_BGN_DT %m-% months(6), LINE_SRVC_BGN_DT) ~ 1, TRUE ~ 0)]

saveRDS(claims, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims_exclude_OD.rds")
