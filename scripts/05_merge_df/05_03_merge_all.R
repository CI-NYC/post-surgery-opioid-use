# -------------------------------------
# Script: merge_all
# Author: Anton Hung 2024_05_31
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)

source("~/medicaid/post_surgery_opioid_use/R/helpers.R")

exposures <- load_data("exposures_merged.fst", file.path(drv_root, "final"))

confounders <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/confounders_merged.rds")

outcomes <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/outcomes_wide_6mos_revised.rds") |>
  select(-oud_hillary_dt,
         -oud_dt,
         -moud_start_dt)

combined_df <- exposures |>
  left_join(confounders) |>
  left_join(outcomes)

c_section_codes <- c("59510","59514","59515")

combined_df_1 <- combined_df |>
  filter(!PRCDR_CD %in% c_section_codes) |>
  select(-PRCDR_CD)

combined_df_2 <- combined_df |>
  filter(PRCDR_CD %in% c_section_codes) |>
  filter(!SEX_M == 1) |>
  select(-PRCDR_CD)


write_data(combined_df_1, "df_non_c_section.fst", file.path(drv_root, "final"))
write_data(combined_df_2, "df_only_c_section.fst", file.path(drv_root, "final"))

# saveRDS(combined_df_1, "/mnt/general-data/disability/post_surgery_opioid_use/final/df_non_c_section.rds")
# saveRDS(combined_df_2, "/mnt/general-data/disability/post_surgery_opioid_use/final/df_only_c_section.rds")
