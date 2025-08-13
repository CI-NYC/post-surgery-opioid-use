# -------------------------------------
# Script: new_moud
# Author: Anton Hung 2024-05-16
# Purpose: combined the three MOUD into a single variable
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)

load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/intermediate"
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/outcomes"

# load cohort
cohort <- readRDS(file.path(load_dir, "first_surgeries.rds")) |>
  select(BENE_ID, followup_start_dt = discharge_dt) |>
  as.data.table()

met <- readRDS(file.path(load_dir, "all_moud_met.rds")) |> 
  as.data.table()

nal <- readRDS(file.path(load_dir, "all_moud_nal.rds")) |> 
  as.data.table()

bup <- readRDS(file.path(load_dir, "all_moud_bup.rds")) |> 
  as.data.table()

all_moud <- rbind(met, nal, bup)

# MOUD
moud_cohort <- left_join(cohort, all_moud, by="BENE_ID")

moud_cohort[, has_moud := 
              as.numeric(moud_start_dt %within% interval(followup_start_dt, followup_start_dt %m+% years(2)))] 

moud_cohort <- moud_cohort |>
  mutate(has_moud = fifelse(is.na(has_moud), 0, has_moud), # converting NA to 0
         moud_start_dt = fifelse(has_moud == 0, NA, moud_start_dt)) |> # ineligible dates are masked
  group_by(BENE_ID) |>
  mutate(has_new_moud = as.numeric(any(has_moud == 1))) |>
  arrange(moud_start_dt) |>
  summarise(has_new_moud = first(has_new_moud),
            moud_start_dt = first(moud_start_dt))

saveRDS(moud_cohort, file.path(save_dir, "cohort_has_new_moud.rds"))


