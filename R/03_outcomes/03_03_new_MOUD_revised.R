# -------------------------------------
# Script: new_moud
# Author: Anton Hung 2024-05-16
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)

# load cohort
cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds") |>
  select(BENE_ID, followup_start_dt) |>
  as.data.table()

met <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_met.rds") |> 
  as.data.table()

nal <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_nal.rds") |> 
  as.data.table()

bup <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_moud_bup.rds") |> 
  as.data.table()

all_moud <- rbind(met, nal, bup)


# MET
moud_cohort <- left_join(cohort, all_moud, by="BENE_ID")

moud_cohort[, has_moud := 
              as.numeric(moud_start_dt %within% interval(followup_start_dt, followup_start_dt %m+% years(2)))] 

moud_cohort <- moud_cohort |>
  mutate(has_moud = case_when(is.na(has_moud) ~ 0, TRUE ~ has_moud), # converting NA to 0
         moud_start_dt = case_when(has_moud == 0 ~ NA, TRUE ~ moud_start_dt)) |> # ineligible dates are masked
  group_by(BENE_ID) |>
  mutate(has_new_moud = as.numeric(any(has_moud == 1))) |>
  arrange(moud_start_dt) |>
  slice(1) |>
  ungroup() |>
  select(BENE_ID, has_new_moud, moud_start_dt)

saveRDS(moud_cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_moud.rds")


