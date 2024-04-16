# -------------------------------------
# Script: has_surgery
# Author: Anton Hung
# Purpose: Identify which individuals have an eligible surgery claim code
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
library(arrow)
library(yaml)
library(lubridate)

# cohort data
# cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds")
# cohort <- cohort[, "BENE_ID"]

# claims data
src_root <- "/mnt/processed-data/disability"
# drv_root <- "/home/amh2389/medicaid"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# surgery claims
variable = "Surgery"
codes <- read_yaml("/home/amh2389/medicaid/post_surgery_opioid_use/R/surgery_codes.yml")
codes <- c(names(codes[[variable]]$CPT),
           names(codes[[variable]]$ICD10),
           names(codes[[variable]]$CCS))

# Filter OTL to claims codes
claims_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |>
  filter(LINE_PRCDR_CD %in% codes) |>
  filter(!is.na(BENE_ID)) |>
  mutate(LINE_SRVC_END_DT = case_when(is.na(LINE_SRVC_END_DT) ~ LINE_SRVC_BGN_DT, TRUE ~ LINE_SRVC_END_DT)) |>
  collect()

# after this step, there are about 6 thousand repeated CLM_IDs. these observations differ on LINE_ID
# different lines can have different dates or procedures (LINE_PRCDR_CD),
# but as they are all still under the same claim, likely this is not important,

# Decision:
# grouping by claim ID, I set the start date to be the earliest date of any of the observations under the clm_ID, and the end date to be the latest date
# I also set the procedure codes to be the last procedure. These do not play a role in the analysis, so it is just done to ensure that distinct() filters the data down to only the unique claims.

# https://resdac.org/cms-data/variables/claim-line-number
# clm ID can be duplicated across multiple different line_IDs
# grouping each claim id, taking the minimum bgn dt and maximimum end dt
claims <- claims |>
  group_by(CLM_ID) |>
  mutate(LINE_SRVC_BGN_DT = min(LINE_SRVC_BGN_DT),
         LINE_SRVC_END_DT = max(LINE_SRVC_END_DT),
         LINE_PRCDR_CD_SYS = last(LINE_PRCDR_CD_SYS),
         LINE_PRCDR_CD = last(LINE_PRCDR_CD)) |> # taking the last procedure that they got under that claim ID
  distinct() |>
  ungroup()
  


# not done, there are some who differ on CODE
# e.g.: 
# BENE_ID          CLM_ID LINE_SRVC_BGN_DT LINE_SRVC_END_DT LINE_PRCDR_CD_SYS LINE_PRCDR_CD
# <char>          <char>           <Date>           <Date>            <char>        <char>
#   1: HHHHHHHdA4nd4CA HHHHd4C7eAennB7       2018-08-30       2018-08-30                01         46255
# 2: HHHHHHHdA4nd4CA HHHHd4C7eAennB7       2018-08-30       2018-08-30                01         46945


saveRDS(claims, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")
