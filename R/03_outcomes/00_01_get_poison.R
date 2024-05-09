# -------------------------------------
# Script: get_poison
# Author: Anton Hung
# Purpose: modification of 06_define_OUD_components/define_oud_poison.R (by Kat Hoffman)
#          This script finds and saves all poison dates, not just the first one
# Notes:
# -------------------------------------

# load libraries
library(arrow)
library(tidyverse)
library(tidylog)
library(lubridate)
library(data.table)
library(furrr)


surgeries <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")

# otl and rxl data for finding opioid claims
src_root <- "/mnt/processed-data/disability"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFIPH", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
iph <- open_dataset(file.path(src_root, parquet_files))

# Read in RXL (pharmacy line)
files <- paste0(list.files(src_root, pattern = "TAFOTH", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
oth <- open_dataset(file.path(src_root, parquet_files))

# Excluded intentional codes (suicide)
cochran_codes_poison <-
  c(
    "T400X1A", #  Poisoning by opium, accidental (unintentional), initial encounter
    # "T400X2A", # Poisoning by opium, intentional self-harm, initial encounter
    "T400X3A", # Poisoning by opium, assault, initial encounter
    "T400X4A", # Poisoning by opium, undetermined, initial encounter
    "T403X1A", # Poisoning by methadone, accidental (unintentional), initial encounter
    # "T403X2A", # Poisoning by methadone, intentional self-harm, initial encounter
    "T403X3A", # Poisoning by methadone, assault, initial encounter
    "T403X4A", # Poisoning by methadone, undetermined, initial encounter
    "T402X1A", # Poisoning by other opioids, accidental (unintentional), initial encounter
    # "T402X2A", # Poisoning by other opioids, intentional self-harm, initial encounter
    "T402X3A", # Poisoning by other opioids, assault, initial encounter
    "T402X4A", # Poisoning by other opioids, undetermined, initial encounter
    "T404X1A", # Poisoning by other synthetic narcotics, accidental (unintentional), initial encounter
    # "T404X2A", # Poisoning by other synthetic narcotics, intentional self-harm, initial encounter
    "T404X3A", # Poisoning by other synthetic narcotics, assault, initial encounter
    "T404X4A", # Poisoning by other synthetic narcotics, undetermined, initial encounter
    "T40601A", # Poisoning by unspecified narcotics, accidental (unintentional), initial encounter
    # "T40602A", # Poisoning by unspecified narcotics, intentional self-harm, initial encounter
    "T40603A", # Poisoning by unspecified narcotics, assault, initial encounter
    "T40604A", # Poisoning by unspecified narcotics, undetermined, initial encounter
    "T40691A", # Poisoning by other narcotics, accidental (unintentional), initial encounter
    # "T40692A", # Poisoning by other narcotics, intentional self-harm, initial encounter
    "T40693A", # Poisoning by other narcotics, assault, initial encounter
    "T40694A" # Poisoning by other narcotics, undetermined, initial encounter
  )



# Filter arrow files by cohort ---------------------------------------------------------

oth_cohort <- # first extract the other services data
  oth |>
  filter(!is.na(DGNS_CD_1)) |>
  filter(BENE_ID %in% surgeries$BENE_ID) |> 
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  filter(SRVC_BGN_DT >= as.Date("2016-01-01")) |> # save space, keep only DG codes after 2016
  select(BENE_ID, SRVC_BGN_DT,  SRVC_END_DT, contains("DGNS_CD")) |>
  collect()

iph_cohort <- # then extract IPH data
  iph |>
  filter(BENE_ID %in% surgeries$BENE_ID) |> 
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  filter(SRVC_BGN_DT >= as.Date("2016-01-01")) |> # save space, keep only DG codes after 2016
  select(BENE_ID, ADMSN_DT, SRVC_BGN_DT, contains("DGNS_CD")) |>
  collect()



# Create OUD component date variables from IPH and OTH ---------------------------------------------------------


oth_all_vars <-
  oth_cohort |> # define oud variable of interest via codes
  mutate(oud_poison_dt = case_when(DGNS_CD_1 %in% cochran_codes_poison ~ SRVC_BGN_DT,
                                   DGNS_CD_2 %in% cochran_codes_poison ~ SRVC_BGN_DT)) |>
  drop_na(oud_poison_dt) |> # drop anyone who doesn't have a date for the oud var of interest
  distinct(BENE_ID, oud_poison_dt)

iph_all_vars <-
  iph_cohort |> # for IPH, first need to pivot to long format because there are 12 DG codes
  pivot_longer(cols = contains("DGNS_CD"), names_to = "dg_num", values_to = "cd") |> # switch to long format data because there are 12 dx codes and IPH isn't too big
  drop_na(cd) |> # drop any missing icd code (cd) rows
  mutate(oud_poison_dt = case_when(cd %in% cochran_codes_poison ~ SRVC_BGN_DT)) |>  # define oud variable of interest via codes
  drop_na(oud_poison_dt) |> # drop anyone who doesn't have a date for the oud var of interest
  distinct(BENE_ID, oud_poison_dt)

all <- bind_rows(oth_all_vars, iph_all_vars) # stack the columns (BENE_ID and oud var of interest) on top of each other



saveRDS(all, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_poison_dts.rds")
