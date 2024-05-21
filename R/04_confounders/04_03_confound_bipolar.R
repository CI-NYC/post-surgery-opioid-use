# -------------------------------------
# Script: 04_03_confound_bipolar
# Author: Anton Hung 2024-05-20
# Purpose:
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

src_root <- "/mnt/processed-data/disability"

# Read in IPH 
files <- paste0(list.files(src_root, pattern = "TAFIPH", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
iph <- open_dataset(file.path(src_root, parquet_files))

# Read in OTH
files <- paste0(list.files(src_root, pattern = "TAFOTH", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
oth <- open_dataset(file.path(src_root, parquet_files))

# read in all icd anxiety codes
bipolar_icds <- read_csv("~/medicaid/post_surgery_opioid_use/R/04_confounders/bipolar_icd10_20230323.csv", col_names = F) |>
  rename(ICD9_OR_10 = X1)

############################################################################
############################################################################
# Step 1: in parallel across the 17 beneficiary splits, extract OTH codes and 
#       keep only the diagnosis codes (1 and 2, separately) which are in the bipolar
#       ICD code list, save as a "raw_i.parquet tmp split file
############################################################################
############################################################################

ids <- cohort |> pull(BENE_ID)
dg1 <- 
  oth |> 
  filter(BENE_ID %in% ids) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  select(BENE_ID, SRVC_BGN_DT, DGNS_CD_1) |>
  rename(dgcd = DGNS_CD_1) |>
  filter(dgcd %in% bipolar_icds$ICD9_OR_10) |>
  arrange(SRVC_BGN_DT) |>
  collect() 
dg2 <- 
  oth |> 
  filter(BENE_ID %in% ids) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  select(BENE_ID, SRVC_BGN_DT, DGNS_CD_2) |>
  rename(dgcd = DGNS_CD_2) |>
  filter(dgcd %in% bipolar_icds$ICD9_OR_10) |>
  arrange(SRVC_BGN_DT) |>
  collect()
all_dg <- bind_rows(dg1, dg2)


############################################################################
############################################################################
# Step 2: in parallel across the 17 beneficiary splits, extract OTH codes and 
#       that occur after the washout period begins, and only keep the minimum
#       ICD code list, save as a "i.parquet tmp split file
############################################################################
############################################################################

all_dg_clean <-
  all_dg |>
  left_join(cohort |> select(BENE_ID, washout_start_dt)) |>
  group_by(BENE_ID) |>
  filter(SRVC_BGN_DT >= washout_start_dt) |>
  summarize(min_bipolar_dt = min(SRVC_BGN_DT)) |>
  ungroup()


############################################################################
############################################################################
# Step 3: extract bipolar ICD codes from the Inpatient Hospital files
############################################################################
############################################################################

icd_codes_to_check <-
  iph |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  select(BENE_ID, SRVC_BGN_DT, contains("DGNS_CD")) |>
  collect()

iph_dg <-
  icd_codes_to_check |>
  mutate(bipolar = +(if_any(starts_with("DGNS_CD"),  ~. %in% bipolar_icds$ICD9_OR_10))) |>
  filter(bipolar == T) |> # only keep bipolar codes
  left_join(cohort |> select(BENE_ID, washout_start_dt)) |> # join washout start date in
  group_by(BENE_ID) |>
  filter(SRVC_BGN_DT >= washout_start_dt) |>  # only keep begin dates that occur on or after washout date
  summarize(min_bipolar_dt = min(SRVC_BGN_DT)) # only keep the minimum date


############################################################################
############################################################################
# Step 4: in parallel across the 17 OTH splits, left join the IPH file
#   keep only the minimum bipolar date between OTH and IPH for that beneficiary
#   save as i_clean.parquet in temp folder
############################################################################
############################################################################

clean <- 
  all_dg_clean |>
  left_join(iph_dg) |>
  left_join(cohort |> select(BENE_ID, washout_start_dt)) |>
  filter(min_bipolar_dt >= washout_start_dt) |> # only keep bipolar dg codes after washout starts
  group_by(BENE_ID) |>
  summarize(bipolar_dt = min(min_bipolar_dt)) |>
  distinct() # some dates are duplicated


############################################################################
############################################################################
# Step 5: add in beneficiaries minimum dates that were only in IPH, not OTH
############################################################################
############################################################################

# pull out beneficiaries that we don't already have in OTH
iph_only <-
  iph_dg |>
  filter(!(BENE_ID %in% clean$BENE_ID)) |>
  rename(bipolar_dt = min_bipolar_dt)

# bind all the rows together (bene_id, depression_dt)
all_bipolar <-
  bind_rows(clean, iph_only) |>
  arrange(bipolar_dt) |>
  distinct(BENE_ID, .keep_all = T)

# saveRDS(all_bipolar, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confounder_bipolar.rds") # save final data file

cohort <- merge(cohort, all_bipolar, by="BENE_ID")

cohort <- cohort |>
  mutate(has_bipolar = as.numeric(bipolar_dt < followup_start_dt)) |>
  select(BENE_ID, has_bipolar)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confounder_bipolar.rds")