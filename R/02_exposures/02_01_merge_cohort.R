# -------------------------------------
# Script: merge_cohort
# Author: Anton Hung
# Purpose: After exclusions have been applied, I am creating the resulting cohort that we will proceed with in our analysis
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)

setwd("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/")

claims <- readRDS("surgery_claims_with_opioids.rds") |> as.data.frame()

exclusion_poison <- readRDS("cohort_exclusion_poison.rds")
exclusion_oud_hillary <- readRDS("cohort_exclusion_hillary.rds")
exclusion_noncontinuous <- readRDS("cohort_exclusion_noncontinuous.rds")
exclusion_noncontinuous <- (exclusion_noncontinuous) |> group_by(CLM_ID) |> ungroup() # this rds file was accidentally grouped by CLM_ID during my first run-through of this, and this script would error-out. The problem has since been fixed in upstream files, and this line is not strictly necessary.

exclusion_opioids <- readRDS("cohort_exclusion_opioids.rds")
exclusion_age <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdebse/cohort_exclusion_age.rds") |> 
  filter(BENE_ID %in% claims$BENE_ID) |>
  select(BENE_ID, cohort_exclusion_age)




joined_surgeries <- claims |>
  left_join(exclusion_noncontinuous, by = "CLM_ID") |>
  left_join(exclusion_opioids, by = "CLM_ID") |>
  left_join(exclusion_poison, by = "CLM_ID") |>
  left_join(exclusion_oud_hillary, by = "CLM_ID") |>
  left_join(exclusion_age, by = "BENE_ID")

saveRDS(joined_surgeries, "joined_surgeries.rds") # pre data cleaning

cleaned_surgeries <- joined_surgeries |>
  mutate(cohort_exclusion_cal = 
           cohort_exclusion_noncontinuous +
           cohort_exclusion_opioids +
           cohort_exclusion_poison +
           cohort_exclusion_oud_hillary +
           cohort_exclusion_age)

cleaned_surgeries <- cleaned_surgeries |>
  filter(cohort_exclusion_cal == 0)

saveRDS(cleaned_surgeries, "cleaned_surgeries.rds")
