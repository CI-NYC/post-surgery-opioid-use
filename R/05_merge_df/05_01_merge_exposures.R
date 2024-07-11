# -------------------------------------
# Script: merge_exposures
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)

# cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

opioid_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/opioid_data"

mme <- readRDS(file.path(opioid_dir, "surgery_mean_daily_dose_mme_truncated.rds"))
days_supplied <- readRDS(file.path(opioid_dir, "surgery_opioid_days_supplied.rds"))
days_continuous <- readRDS(file.path(opioid_dir, "surgery_opioids_days_continuous.rds"))


cohort <- mme |>
  left_join(days_supplied) |>
  left_join(days_continuous)

cohort$days_of_continuous_use <- sapply(cohort$days_of_continuous_use, max)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/final/exposures_merged.rds")
