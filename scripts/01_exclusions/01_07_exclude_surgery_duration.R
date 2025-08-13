# -------------------------------------
# Script: exclusion_surgery_duration
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)
library(tidyverse)

# load all the surgeries
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds") |>
  mutate(cohort_exclusion_surgery_duration = case_when(discharge_dt - surgery_dt >= 7 ~ 1, TRUE ~ 0)) |>
  select(BENE_ID, CLM_ID, cohort_exclusion_surgery_duration)

saveRDS(claims, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_surgery_duration.rds")


