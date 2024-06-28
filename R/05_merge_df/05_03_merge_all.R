# -------------------------------------
# Script: merge_all
# Author: Anton Hung 2024_05_31
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)

exposures <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/exposures_merged.rds")

confounders <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/confounders_merged.rds")

outcomes <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/outcomes_wide_6mos.rds")[,-c(3:8)]

combined_df <- exposures |>
  left_join(confounders) |>
  left_join(outcomes)

c_section_codes <- c("59510","59514","59515")

combined_df_1 <- combined_df |>
  filter(!PRCDR_CD %in% c_section_codes) |>
  select(-PRCDR_CD)

combined_df_2 <- combined_df |>
  filter(PRCDR_CD %in% c_section_codes) |>
  select(-PRCDR_CD)



saveRDS(combined_df_1, "/mnt/general-data/disability/post_surgery_opioid_use/final/df_non_c_section.rds")
saveRDS(combined_df_2, "/mnt/general-data/disability/post_surgery_opioid_use/final/df_only_c_section.rds")
