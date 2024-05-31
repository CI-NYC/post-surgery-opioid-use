# -------------------------------------
# Script: confounders
# Author: Anton Hung 2024-05-17
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
age <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_age.rds") |> select(BENE_ID, CLM_ID, age_enrollment)
sex <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds") |>
  select(BENE_ID, SEX_CD) |>
  mutate(SEX_M = as.numeric(SEX_CD == "M")) |>
  select(BENE_ID, SEX_M)

confound_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/confounders"

anxiety <- readRDS(file.path(confound_dir, "confounder_anxiety.rds"))
depression <- readRDS(file.path(confound_dir, "confounder_depression.rds"))
bipolar <- readRDS(file.path(confound_dir, "confounder_bipolar.rds"))
substance <- readRDS(file.path(confound_dir, "confounder_alc_subst_smoke.rds"))
pain <- readRDS(file.path(confound_dir, "confounder_pain.rds"))
surgery_type <- readRDS(file.path(confound_dir, "confound_major_surgery.rds"))

cohort <- cohort |>
  left_join(age, by = c("BENE_ID", "CLM_ID")) |>
  select(BENE_ID, age_enrollment) |>
  left_join(sex, by = "BENE_ID") |>
  left_join(anxiety, by = "BENE_ID") |>
  left_join(depression, by = "BENE_ID") |>
  left_join(bipolar, by = "BENE_ID") |>
  left_join(substance, by = "BENE_ID") |>
  left_join(pain, by = "BENE_ID") |>
  mutate(has_anxiety = case_when(is.na(has_anxiety) ~ 0, TRUE ~ has_anxiety),
         has_depression = case_when(is.na(has_depression) ~ 0, TRUE ~ has_depression),
         has_bipolar = case_when(is.na(has_bipolar) ~ 0, TRUE ~ has_bipolar))


saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/final/confounders_merged.rds")



