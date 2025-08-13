# -------------------------------------
# Script: has_surgery
# Author: Anton Hung
# Purpose: Identify which beneficiaries have an eligible surgery claim code
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
library(arrow)
library(yaml)
library(lubridate)
library(tidyverse)


# claims data
src_root <- "/mnt/processed-data/disability"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# Read in IPH (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFIPH", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
iph <- open_dataset(file.path(src_root, parquet_files))

# surgery claims
variable = "Surgery"
codes <- read_yaml("/home/amh2389/medicaid/post_surgery_opioid_use/input/surgery_codes.yml")
codes <- c(names(codes[[variable]]$CPT),
           names(codes[[variable]]$ICD10))

# Filter OTL to claims codes
otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD")
otl <- select(otl, all_of(otl_vars)) |>
  filter(LINE_PRCDR_CD %in% codes) |>
  filter(!is.na(BENE_ID)) |>
  mutate(LINE_SRVC_END_DT = case_when(is.na(LINE_SRVC_END_DT) ~ LINE_SRVC_BGN_DT, TRUE ~ LINE_SRVC_END_DT)) |>
  collect()

# after this step, there are about 6 thousand repeated CLM_IDs. these observations differ on LINE_ID
# different lines can have different dates or procedures (LINE_PRCDR_CD),
# but as they are all still under the same claim, likely this is not important,

# Decision:
# grouping by claim ID, I set the start date to be the earliest date of any of the observations under the clm_ID, and the end date to be the latest date
# I also combine all the procedure codes belonging to the same CLAIM into a list

# https://resdac.org/cms-data/variables/claim-line-number
# clm ID can be duplicated across multiple different line_IDs
# grouping each claim id, taking the minimum bgn dt and maximimum end dt

# e.g.: 
# BENE_ID          CLM_ID LINE_SRVC_BGN_DT LINE_SRVC_END_DT LINE_PRCDR_CD_SYS LINE_PRCDR_CD
# <char>          <char>           <Date>           <Date>            <char>        <char>
#   1: HHHHHHHdA4nd4CA HHHHd4C7eAennB7       2018-08-30       2018-08-30                01         46255
# 2: HHHHHHHdA4nd4CA HHHHd4C7eAennB7       2018-08-30       2018-08-30                01         46945

otl <- otl |>
  group_by(CLM_ID) |>
  mutate(SRVC_BGN_DT = min(LINE_SRVC_BGN_DT),
         SRVC_END_DT = max(LINE_SRVC_END_DT),
         PRCDR_CD = list(unique(LINE_PRCDR_CD))) |> # combining all procedure codes into a list
  slice(1) |>
  ungroup() |>
  select(BENE_ID, CLM_ID, SRVC_BGN_DT, SRVC_END_DT, PRCDR_CD)
  

##### Searching IPH file for surgeries

iph_vars <- c("BENE_ID", "CLM_ID", "SRVC_BGN_DT", "SRVC_END_DT", "ADMSN_DT", "DSCHRG_DT", "PRCDR_CD_1","PRCDR_CD_2", "PRCDR_CD_3", "PRCDR_CD_4", "PRCDR_CD_5", "PRCDR_CD_6")
              # "PRCDR_CD_DT_1", "PRCDR_CD_DT_2", "PRCDR_CD_DT_3", "PRCDR_CD_DT_4", "PRCDR_CD_DT_5", "PRCDR_CD_DT_6")

iph <- select(iph, all_of(iph_vars)) |>
  filter(if_any(starts_with("PRCDR_CD"),  ~. %in% codes)) |>
  filter(!is.na(BENE_ID)) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect() |>
  mutate(SRVC_BGN_DT = fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT),
         maxdate = pmax(SRVC_BGN_DT, SRVC_END_DT, ADMSN_DT, DSCHRG_DT,
                        # PRCDR_CD_DT_1, PRCDR_CD_DT_2, PRCDR_CD_DT_3, PRCDR_CD_DT_4, PRCDR_CD_DT_5, PRCDR_CD_DT_6,
                        na.rm = TRUE),
         SRVC_END_DT = fifelse(SRVC_BGN_DT > SRVC_END_DT, maxdate, SRVC_END_DT)) |>
  select(BENE_ID, CLM_ID, SRVC_BGN_DT, SRVC_END_DT, PRCDR_CD_1, PRCDR_CD_2, PRCDR_CD_3, PRCDR_CD_4, PRCDR_CD_5, PRCDR_CD_6)
  
iph <- iph |>
  pivot_longer(cols = starts_with("PRCDR_CD"),
               names_to = "PRCDR_num",
               values_to = "PRCDR_CD") |>
  filter(PRCDR_CD %in% codes) |>
  group_by(CLM_ID) |>
  mutate(PRCDR_CD = list(PRCDR_CD)) |>
  slice(1) |>
  ungroup() |>
  select(BENE_ID, CLM_ID, SRVC_BGN_DT, SRVC_END_DT, PRCDR_CD)


# calculate a washout period of 6 months before surgery
claims <- otl |>
  rbind(iph) |>
  rename(surgery_dt = SRVC_BGN_DT,
         discharge_dt = SRVC_END_DT) |>
  mutate(washout_start_dt = surgery_dt %m-% days(182)) |>
  relocate(washout_start_dt, .before = surgery_dt)


saveRDS(claims, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")


