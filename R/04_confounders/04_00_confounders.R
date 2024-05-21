# -------------------------------------
# Script: confounders
# Author: Anton Hung 2024-05-17
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
age <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_age.rds") |> select(BENE_ID, CLM_ID, age_enrollment)
sex <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df") |> select(BENE_ID, SEX_CD)


cohort <- cohort |>
  left_join(age, by = c("BENE_ID", "CLM_ID")) |>
  left_join(sex, by=BENE_ID)


