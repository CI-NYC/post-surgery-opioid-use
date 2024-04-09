# -------------------------------------
# Script: setup
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(dplyr)
library(data.table)
library(arrow)
library(yaml)
library(lubridate)

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds")
# cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")

cohort <- cohort[, c("BENE_ID",
                     "cohort_exclusion_age",
                     "age_enrollment",
                     "SEX_CD",
                     "anxiety_washout_cal",
                     "depression_washout_cal",
                     "bipolar_washout_cal",
                     "oud_poison_dt"
                     # other substnace use disorder
                     # smoking history
)]

src_root <- "/mnt/processed-data/disability"
drv_root <- "/home/amh2389/medicaid"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))