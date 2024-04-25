# -------------------------------------
# Script: Define MOUD (buprenorphine)
# Author: Kat Hoffman July 2023
# Purpose:
# Notes:
# -------------------------------------

# Set up ------------------------------------------------------------------

# load libraries
library(arrow)
library(tidyverse)
library(tidylog)
library(lubridate)
library(data.table)
library(tictoc)
library(here)

met_hcpcs <- c(
  "H0020", "J1230", "G2067", "G2078",
  "S0109" # later criteria
)


surgeries <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")


# otl and rxl data for finding opioid claims
src_root <- "/mnt/processed-data/disability"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))


# pull HCPCS codes --------------------------------------------------------

met_hcpcs_otl <-
  otl |>
  filter(LINE_PRCDR_CD %in% met_hcpcs) |>
  mutate(LINE_SRVC_BGN_DT = case_when(is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, TRUE ~ LINE_SRVC_BGN_DT),
  ) |>
  select(BENE_ID,
         STATE_CD, 
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_PRCDR_CD
  ) |>
  collect() |>
  mutate(year = year(LINE_SRVC_BGN_DT)) |>
  filter((LINE_PRCDR_CD == "S0109" & STATE_CD == "IA" & year == 2016) |
           LINE_PRCDR_CD != "S0109") |>
  mutate(moud_med = "met",
         form = "tablet",
         moud_start_dt = LINE_SRVC_BGN_DT,
         moud_end_dt = moud_start_dt + 21) |> # met implants last 1 day
  select(BENE_ID, moud_med, form, moud_start_dt, moud_end_dt)

# adjudicate the methadone information depending on when the beneficiary received the next dose
# check when the next/last injection was given, and how long that was since the last injection ended (injection + 28 days)
met_adj <-
  met_hcpcs_otl |>
  drop_na(BENE_ID) |>
  mutate(lag_moud_end_dt = lag(moud_end_dt), # when did the last bup end?
         days_since_last_moud = as.integer(difftime(moud_start_dt, lag_moud_end_dt, units="days")),
         lead_moud_start_dt = lead(moud_start_dt), # when is the next dose starting compared to last moud end date
         days_to_next_moud = as.integer(difftime(lead_moud_start_dt, moud_end_dt, units="days")))  |>
  ungroup()|>
  # indicator of whether to use the moud_start_dt/moud_end_dt or ignore them
  mutate(use_start_dt = case_when(is.na(days_since_last_moud) ~ 1,
                                  days_since_last_moud > 0 ~ 1,
                                  TRUE ~ 0),
         
         use_end_dt = case_when(is.na(days_to_next_moud) ~ 1,
                                days_to_next_moud > 0 ~ 1,
                                TRUE ~ 0))

# keep only the needed rows for start dates
met_start_dts <-
  met_adj |>
  filter(use_start_dt == 1) |>
  select(BENE_ID, moud_start_dt) |>
  ungroup()

# keep only the needed rows for end dates
met_end_dts <-
  met_adj |>
  filter(use_end_dt == 1) |>
  select(BENE_ID, moud_end_dt) |>
  ungroup()

# merge to one final bup start/stop data set
all_met_start_stop <-
  met_start_dts |>
  bind_cols(met_end_dts |> select(-BENE_ID)) |>
  mutate(moud_med = "met") |>
  select(BENE_ID, moud_med, everything())|>
  left_join(surgeries |> select(BENE_ID, LINE_SRVC_BGN_DT)) |>
  filter(!(moud_end_dt < LINE_SRVC_BGN_DT)) |> # filter out rows that end before washout period begins
  select(-LINE_SRVC_BGN_DT)


# met_intervals <-
#   all_met_start_stop |>
#   left_join(dts_cohorts) |> # left_join, only merge with rows that have any bup
#   # start or end date can fall in the periods of interst
#   mutate(moud_start_dt = case_when(moud_start_dt < LINE_SRVC_BGN_DT ~ LINE_SRVC_BGN_DT, TRUE ~ moud_start_dt),
#          oud_moud_met_washout_cal = case_when(moud_start_dt %within% interval(LINE_SRVC_BGN_DT, washout_cal_end_dt) ~ 1,
#                                               moud_end_dt %within% interval(LINE_SRVC_BGN_DT, washout_cal_end_dt) ~ 1,
#                                               TRUE ~ 0),
#          oud_moud_met_washout_12mos_cal = case_when(moud_start_dt %within% interval(washout_start_dt, washout_12mos_end_dt) ~ 1,
#                                                     moud_end_dt %within% interval(washout_start_dt, washout_12mos_end_dt) ~ 1,
#                                                     TRUE ~ 0),
#          oud_moud_met_washout_cont = case_when(moud_start_dt %within% interval(washout_start_dt, washout_cont_end_dt) ~ 1,
#                                                moud_end_dt %within% interval(washout_start_dt, washout_cont_end_dt) ~ 1,
#                                                TRUE ~ 0),
#          oud_moud_met_study_cal = case_when(moud_start_dt %within% interval(washout_start_dt, study_cal_end_dt) ~ 1,
#                                             moud_end_dt %within% interval(washout_start_dt, study_cal_end_dt) ~ 1,
#                                             TRUE ~ 0),
#          oud_moud_met_study_cont = case_when(moud_start_dt %within% interval(washout_start_dt, study_cont_end_dt) ~ 1,
#                                              moud_end_dt %within% interval(washout_start_dt, study_cont_end_dt) ~ 1,
#                                              TRUE ~ 0),
#   ) |>
#   group_by(BENE_ID) |>
#   summarize(across(starts_with("oud_moud"), max)) # keep a row for each BENE_ID that denotes whether they had the drug in this period

# all_met_intervals <- 
#   dts_cohorts |>
#   select(BENE_ID) |>
#   left_join(met_intervals) |>
#   mutate(across(contains("oud_moud"), ~ifelse(is.na(.x), 0, .x))) # replace missing data with zero (no met in that interval)
# 
saveRDS(all_met_start_stop, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries_moud_met.rds")
