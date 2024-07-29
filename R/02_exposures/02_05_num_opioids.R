# -------------------------------------
# Script: get_num_opioids
# Author: Anton Hung
# Purpose: retrieving the number of opioid prescriptions received during the perioperative period
# Notes:
# -------------------------------------
library(data.table)
library(dplyr)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
setDT(cohort)

opioids <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/opioids_for_surgery.rds") |>
  filter(CLM_ID %in% cohort$CLM_ID) |>
  group_by(BENE_ID) |>
  mutate(multiple_opioids = if_else(n() > 1, 1, 0)) |>
  slice(1) |>
  select(BENE_ID, multiple_opioids)

saveRDS(opioids, "/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/multiple_opioids.rds")