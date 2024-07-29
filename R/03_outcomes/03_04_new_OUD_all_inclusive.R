# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
library(lubridate)

all_poison <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_poison_dts.rds") |>
  rename(oud_dt = oud_poison_dt)

all_hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_hillary_dts.rds") |>
  rename(oud_dt = oud_hillary_dt)

all_met <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_met.rds") |> 
  as.data.table() |>
  select(BENE_ID, oud_dt = moud_start_dt)
  
all_nal <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_nal.rds") |> 
  as.data.table() |>
  select(BENE_ID, oud_dt = moud_start_dt)

all_bup <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_bup.rds") |> 
  as.data.table() |>
  select(BENE_ID, oud_dt = moud_start_dt)

all_oud <- rbind(all_poison,
                 all_hillary,
                 all_met,
                 all_nal,
                 all_bup)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds") |>
  select(BENE_ID, followup_start_dt) |>
  as.data.table()



cohort <- cohort |>
  left_join(all_oud)

cohort[, has_oud := 
                as.numeric(oud_dt %within% interval(followup_start_dt, followup_start_dt %m+% years(2)))] 

cohort <- cohort |>
  mutate(has_oud = case_when(is.na(has_oud) ~ 0, TRUE ~ has_oud), # converting NA to 0
         oud_dt = case_when(has_oud == 0 ~ NA, TRUE ~ oud_dt)) |> # ineligible dates are masked
  group_by(BENE_ID) |>
  mutate(has_new_oud = as.numeric(any(has_oud == 1))) |>
  arrange(oud_dt) |>
  slice(1) |>
  ungroup() |>
  select(BENE_ID, has_new_oud, oud_dt)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_oud.rds")
