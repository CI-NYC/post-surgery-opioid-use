# -------------------------------------
# Script: merge_all
# Author: Anton Hung 2024_05_31
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
source("~/medicaid/post_surgery_opioid_use/R/helpers.R")

original_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/final"
load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/outcomes"
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/final"

# these two are unchanged from the primary analysis
exposures <- load_data("exposures_merged.fst", file.path(drv_root, "final"))
confounders <- readRDS(file.path(original_dir, "confounders_merged.rds"))

# this one is changed for the sensitivity analysis
outcomes <- readRDS(file.path(load_dir, "outcomes_wide_6mos_revised.rds")) |>
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

write_data(combined_df_1, "df_non_c_section.fst", file.path(drv_root, "sensitivity_analysis/final"))
write_data(combined_df_2, "df_only_c_section.fst", file.path(drv_root, "sensitivity_analysis/final"))

# saveRDS(combined_df_1, file.path(save_dir, "df_non_c_section.rds"))
# saveRDS(combined_df_2, file.path(save_dir, "df_only_c_section.rds"))
# 
