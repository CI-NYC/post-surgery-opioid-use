# -------------------------------------
# Script: confound_substance_use
# Author: Anton Hung 2024-05-20
# Purpose:
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(arrow)
library(data.table)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

# claims data
src_root <- "/mnt/processed-data/disability"

# Read in OTH
files <- paste0(list.files(src_root, pattern = "TAFOTH", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
oth <- open_dataset(file.path(src_root, parquet_files))

# Read in IPH
files <- paste0(list.files(src_root, pattern = "TAFIPH", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
iph <- open_dataset(file.path(src_root, parquet_files))


# Filter claims codes
### ALCOHOL
alcohol_oth <- oth |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_1) |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter(grepl("^F101", DGNS_CD_1) | grepl("^F102", DGNS_CD_1)) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect()


alcohol_iph <- iph |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_1) |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter(grepl("^F101", DGNS_CD_1) | grepl("^F102", DGNS_CD_1)) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect()

has_alcohol_abuse <- rbind(alcohol_oth, alcohol_iph) |>
  right_join(cohort) |>
  filter(SRVC_BGN_DT %within% interval(washout_start_dt, followup_start_dt)) |>
  group_by(BENE_ID) |>
  mutate(has_alcohol_abuse = 1) |>
  select(BENE_ID, has_alcohol_abuse) |>
  ungroup() |>
  distinct()


### OTHER SUBSTANCE ABUSE
# Filter claims codes
substance_oth <- oth |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_1) |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter(grepl("^F1[1-9]1", DGNS_CD_1) | grepl("^F1[1-9]2", DGNS_CD_1)) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect()

substance_iph <- iph |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_1) |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter(grepl("^F1[1-9]1", DGNS_CD_1) | grepl("^F1[1-9]2", DGNS_CD_1)) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect()

has_substance_abuse <- rbind(substance_oth, substance_iph) |>
  right_join(cohort) |>
  filter(SRVC_BGN_DT %within% interval(washout_start_dt, followup_start_dt)) |>
  group_by(BENE_ID) |>
  mutate(has_substance_abuse = 1) |>
  select(BENE_ID, has_substance_abuse) |>
  ungroup() |>
  distinct()


### SMOKING HISTORY
smoking_oth <- oth |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_1) |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter(DGNS_CD_1 %in% c("Z87891", "F17200")) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect()

smoking_iph <- iph |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_1) |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter(DGNS_CD_1 %in% c("Z87891", "F17200")) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect()

has_smoking_history <- rbind(smoking_oth, smoking_iph) |>
  right_join(cohort) |>
  filter(SRVC_BGN_DT %within% interval(washout_start_dt, followup_start_dt)) |>
  group_by(BENE_ID) |>
  mutate(has_smoking_history = 1) |>
  select(BENE_ID, has_smoking_history) |>
  ungroup() |>
  distinct()


### Putting all three together
cohort <- cohort |>
  select(BENE_ID) |>
  left_join(has_alcohol_abuse) |>
  left_join(has_substance_abuse) |>
  left_join(has_smoking_history) |>
  mutate(has_alcohol_abuse = case_when(is.na(has_alcohol_abuse) ~ 0, TRUE ~ has_alcohol_abuse),
         has_substance_abuse = case_when(is.na(has_substance_abuse) ~ 0, TRUE ~ has_substance_abuse),
         has_smoking_history = case_when(is.na(has_smoking_history) ~ 0, TRUE ~ has_smoking_history))

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confounder_alc_subst_smoke.rds")

