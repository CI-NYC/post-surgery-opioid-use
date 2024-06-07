# -------------------------------------
# Script: merge_all
# Author: Anton Hung 2024_05_31
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)

exposures <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/exposures_merged.rds")

confounders <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/confounders_merged.rds")

outcomes <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/outcomes_wide2.rds")[,-c(2:9)]

combined_df <- exposures |>
  left_join(confounders) |>
  left_join(outcomes)

saveRDS(combined_df, "/mnt/general-data/disability/post_surgery_opioid_use/final/combined_df.rds")
