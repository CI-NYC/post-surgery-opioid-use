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

codebook <- read_yaml("/home/amh2389/medicaid/post_surgery_opioid_use/input/surgery_codes.yml")

find_substance <- function(which_substance, new_column_name) {
  codes <- c(names(codebook[[which_substance]]$ICD10))
  
  substance_oth <- oth |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
    filter(BENE_ID %in% cohort$BENE_ID) |>
    filter(if_any(starts_with("DGNS_CD"), ~. %in% codes)) |>
    mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
    collect() |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT)
  
  substance_iph <- iph |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
    filter(BENE_ID %in% cohort$BENE_ID) |>
    filter(if_any(starts_with("DGNS_CD"), ~. %in% codes)) |>
    mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
    collect() |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT)
  
  has_substance_abuse <- rbind(substance_oth, substance_iph) |>
    right_join(cohort) |>
    filter(SRVC_BGN_DT %within% interval(washout_start_dt, followup_start_dt)) |>
    group_by(BENE_ID) |>
    mutate(!!new_column_name := 1) |>
    select(BENE_ID, !!new_column_name) |>
    ungroup() |>
    distinct()
}

### ALCOHOL ABUSE
has_alcohol_abuse <- find_substance("Alcohol", "has_alcohol_abuse")

### OTHER SUBSTANCE ABUSE
has_substance_abuse <- find_substance("Other substances", "has_substance_abuse")

### SMOKING HISTORY
has_smoking_history <- find_substance("Smoking", "has_smoking_history")



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

