# -------------------------------------
# Script: confound_pain
# Author: Anton Hung 2024-05-20
# Purpose:
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(tidylog)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

pain_all <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tmp/pain_all.rds")

chronic_pain_icds <- read.csv("/home/amh2389/medicaid/medicaid-treatment-trends/code/00_make_cohort/chronic_pain_icd10_20230216.csv") |>
  filter(CRITERIA == "Inclusion")


pain_all_adj <- 
  pain_all |>
  left_join(chronic_pain_icds |> select(pain_cat = PAIN_CAT,
                                        dgcd = ICD9_OR_10)) |>
  select(BENE_ID, pain_cat, dgcd_dt) |>
  distinct()


has_pain <- cohort |>
  select(BENE_ID, washout_start_dt, followup_start_dt) |>
  left_join(pain_all_adj, by="BENE_ID") |>
  filter(dgcd_dt %within% interval(washout_start_dt, followup_start_dt)) |>
  group_by(BENE_ID) |>
  mutate(has_pain_back = any(pain_cat == "Back Pain"),
         has_pain_neck = any(pain_cat == "Neck Pain"),
         has_pain_arthritis = any(pain_cat == "Arthritis/Joint/Bone Pain (Other than Back/Neck)"),
         has_pain_neuro = any(pain_cat == "Neurologic Pain"),
         has_pain_headache = any(pain_cat == "Headache"),
         has_pain_misc = any(pain_cat == "Misc Pain"),
         has_pain_back_neck_unspecified = any(pain_cat == "Back/Neck Pain Unspecified")) |>
  select(BENE_ID, has_pain_back, has_pain_neck, has_pain_arthritis, has_pain_neuro, has_pain_headache, has_pain_misc, has_pain_back_neck_unspecified) |>
  ungroup() |>
  distinct()
  


cohort <- cohort |>
  select(BENE_ID) |>
  left_join(has_pain) |>
  mutate(has_pain_back = replace(has_pain_back, is.na(has_pain_back), 0),
         has_pain_neck = replace(has_pain_neck, is.na(has_pain_neck), 0),
         has_pain_arthritis = replace(has_pain_arthritis, is.na(has_pain_arthritis), 0),
         has_pain_neuro = replace(has_pain_neuro, is.na(has_pain_neuro), 0),
         has_pain_headache = replace(has_pain_headache, is.na(has_pain_headache), 0),
         has_pain_misc = replace(has_pain_misc, is.na(has_pain_misc), 0),
         has_pain_back_neck_unspecified = replace(has_pain_back_neck_unspecified, is.na(has_pain_back_neck_unspecified), 0))


saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confounder_pain.rds")
