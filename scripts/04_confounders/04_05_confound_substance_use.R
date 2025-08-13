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
library(yaml)

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
has_other_substance_abuse <- find_substance("Other substances", "has_other_substance_abuse")

### SMOKING HISTORY
has_nicotine_dependance <- find_substance("Smoking", "has_nicotine_dependance")



### Putting all three together
cohort <- cohort |>
  select(BENE_ID) |>
  left_join(has_alcohol_abuse) |>
  left_join(has_other_substance_abuse) |>
  left_join(has_nicotine_dependance) |>
  mutate(has_alcohol_abuse = replace(has_alcohol_abuse, is.na(has_alcohol_abuse), 0),
         has_other_substance_abuse = replace(has_other_substance_abuse, is.na(has_other_substance_abuse), 0),
         has_nicotine_dependance = replace(has_nicotine_dependance, is.na(has_nicotine_dependance), 0))

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confounder_alc_subst_smoke.rds")

