# -------------------------------------
# Script: exclude_age
# Author: Anton Hung 2024-05-15
# Purpose:
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)

surgeries <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")

src_root <- "/mnt/processed-data/disability"

# Read in DBS (demographics)
files <- paste0(list.files(src_root, pattern = "TAFDEBSE", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
dbs <- open_dataset(file.path(src_root, parquet_files))

# keep age and birth date columns for the first year each beneficiary appears in demographics
birth_dates_cols <-
  dbs |>
  arrange(RFRNC_YR) |>
  select(BENE_ID, BIRTH_DT, AGE) |>
  collect()

# keep only the first non NA birth dates
birth_dates <-
  birth_dates_cols |>
  drop_na(BIRTH_DT) |> # 46,894 missing a birth date before doing this
  distinct(BENE_ID, .keep_all = T) 


cohort_exclusion_age <-
  surgeries |> 
  left_join(birth_dates) |>
  # note: this is slightly different from the "AGE" variable calculated by Medicaid (end of year age)
  mutate(age_enrollment = floor(as.numeric(difftime(washout_start_dt, BIRTH_DT, units = "days") / 365.25))) |>
  mutate(cohort_exclusion_age = case_when(age_enrollment < 19 ~ 1,
                                          age_enrollment >= 65 ~ 1, 
                                          TRUE ~ 0)) |>
  select(BENE_ID, CLM_ID, age_enrollment, cohort_exclusion_age)

saveRDS(cohort_exclusion_age, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_age.rds")
