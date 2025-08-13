# -------------------------------------
# Script: merge_exposures
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)

source("~/medicaid/post_surgery_opioid_use/R/helpers.R")

mme <- load_data("exposure_mean_daily_dose_mme.fst", file.path(drv_root, "treatment"))
days_supplied <- load_data("exposure_days_supply.fst", file.path(drv_root, "treatment"))
# days_continuous <- readRDS(file.path(opioid_dir, "surgery_opioids_days_continuous.rds"))
multiple_opioids <- readRDS(file.path(drv_root, "treatment/num_opioids.rds")) |>
  mutate(multiple_opioids = num_opioids > 1) |>
  select(-num_opioids)


cohort <- multiple_opioids |>
  left_join(mme) |>
  left_join(days_supplied) #|>
  # left_join(days_continuous)

# cohort$days_of_continuous_use <- sapply(cohort$days_of_continuous_use, max)

# saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/final/exposures_merged.rds")

write_data(cohort, "exposures_merged.fst", file.path(drv_root, "final"))