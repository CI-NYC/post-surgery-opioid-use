# -------------------------------------
# Script: merge_cohort
# Author: Anton Hung
# Purpose: After exclusions have been applied, I am creating the resulting cohort that we will proceed with in our analysis
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
library(lubridate)

intermediate_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/intermediate"
exclusion_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/exclusion"

claims <- readRDS(file.path(intermediate_dir, "surgery_claims.rds"))

exclusion_poison <- readRDS(file.path(exclusion_dir, "cohort_exclusion_poison.rds"))
exclusion_oud_hillary <- readRDS(file.path(exclusion_dir, "cohort_exclusion_hillary.rds"))
exclusion_noncontinuous <- readRDS(file.path(exclusion_dir, "cohort_exclusion_noncontinuous.rds"))
exclusion_eligible_opioid <- readRDS(file.path(exclusion_dir, "cohort_exclusion_eligible_opioid.rds"))
exclusion_ineligible_opioid <- readRDS(file.path(exclusion_dir, "cohort_exclusion_ineligible_opioid.rds"))
exclusion_surgery_duration <- readRDS(file.path(exclusion_dir, "cohort_exclusion_surgery_duration.rds"))
exclusion_age <- readRDS(file.path(exclusion_dir, "cohort_exclusion_age.rds")) |> select(-age_enrollment)
exclusion_dems <- readRDS(file.path(exclusion_dir, "cohort_exclusion_dems.rds"))
exclusion_moud <- readRDS(file.path(exclusion_dir, "cohort_exclusion_moud.rds"))

# exclusion_age2 <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdebse/cohort_exclusion_age.rds") |> 
#   filter(BENE_ID %in% claims$BENE_ID) |>
#   select(BENE_ID, cohort_exclusion_age)



joined_surgeries <- claims |>
  left_join(exclusion_noncontinuous, by = c("BENE_ID", "CLM_ID")) |>
  left_join(exclusion_eligible_opioid, by = c("BENE_ID", "CLM_ID")) |>
  left_join(exclusion_ineligible_opioid, by = c("BENE_ID", "CLM_ID")) |>
  left_join(exclusion_poison, by = c("BENE_ID", "CLM_ID")) |>
  left_join(exclusion_oud_hillary, by = c("BENE_ID", "CLM_ID")) |>
  left_join(exclusion_surgery_duration, by = c("BENE_ID", "CLM_ID")) |>
  left_join(exclusion_age, by = c("BENE_ID", "CLM_ID")) |>
  left_join(exclusion_dems, by = c("BENE_ID", "CLM_ID")) |>
  left_join(exclusion_moud, by = c("BENE_ID", "CLM_ID")) |>
  mutate(cohort_exclusion_pregnancy = case_when(is.na(cohort_exclusion_pregnancy) ~ 0, TRUE ~ cohort_exclusion_pregnancy),
         cohort_exclusion_cancer_elig = case_when(is.na(cohort_exclusion_cancer_elig) ~ 0, TRUE ~ cohort_exclusion_cancer_elig),
         cohort_exclusion_dual = case_when(is.na(cohort_exclusion_dual) ~ 0, TRUE ~ cohort_exclusion_dual))

saveRDS(joined_surgeries, file.path(intermediate_dir, "joined_surgeries.rds")) # pre data cleaning

cleaned_surgeries <- joined_surgeries |>
  mutate(cohort_exclusion_cal = 
           cohort_exclusion_noncontinuous +
           cohort_exclusion_eligible_opioid +
           cohort_exclusion_ineligible_opioid +
           cohort_exclusion_oud_poison +
           cohort_exclusion_oud_hillary +
           cohort_exclusion_age +
           cohort_exclusion_pregnancy +
           cohort_exclusion_cancer_elig +
           cohort_exclusion_dual +
           cohort_exclusion_moud_met +
           cohort_exclusion_moud_nal +
           cohort_exclusion_moud_bup)

cleaned_surgeries <- cleaned_surgeries |>
  filter(cohort_exclusion_cal == 0)

saveRDS(cleaned_surgeries, file.path(intermediate_dir, "cleaned_surgeries.rds"))

first_surgeries <- cleaned_surgeries |>
  group_by(BENE_ID) |>
  arrange(washout_start_dt) |>
  slice(1) |>
  mutate(followup_start_dt = discharge_dt %m+% days(14)) |>
  relocate(followup_start_dt, .after = discharge_dt)

saveRDS(first_surgeries, file.path(intermediate_dir, "first_surgeries.rds"))
