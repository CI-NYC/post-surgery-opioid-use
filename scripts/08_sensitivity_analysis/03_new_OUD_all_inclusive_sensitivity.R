# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
library(lubridate)

load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/intermediate"
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/outcomes"

# load cohort
cohort <- readRDS(file.path(load_dir, "first_surgeries.rds")) |>
  select(BENE_ID, followup_start_dt = discharge_dt) |>
  as.data.table()

all_poison <- readRDS(file.path(load_dir, "all_poison_dts.rds")) |> 
  as.data.table() |>
  rename(oud_dt = oud_poison_dt)

all_hillary <- readRDS(file.path(load_dir, "all_hillary_dts.rds")) |> 
  as.data.table() |>
  rename(oud_dt = oud_hillary_dt)

all_met <- readRDS(file.path(load_dir, "all_moud_met.rds")) |> 
  as.data.table() |>
  select(BENE_ID, oud_dt = moud_start_dt)

all_nal <- readRDS(file.path(load_dir, "all_moud_nal.rds")) |> 
  as.data.table() |>
  select(BENE_ID, oud_dt = moud_start_dt)

all_bup <- readRDS(file.path(load_dir, "all_moud_bup.rds")) |> 
  as.data.table() |>
  select(BENE_ID, oud_dt = moud_start_dt)

all_oud <- rbind(all_poison,
                 all_hillary,
                 all_met,
                 all_nal,
                 all_bup)

oud_cohort <- cohort |>
  left_join(all_oud)

oud_cohort[, has_oud := 
         as.numeric(oud_dt %within% interval(followup_start_dt, followup_start_dt %m+% years(2)))] 

oud_cohort <- oud_cohort |>
  mutate(has_oud = fifelse(is.na(has_oud), 0, has_oud), # converting NA to 0
         oud_dt = fifelse(has_oud == 0, NA, oud_dt)) |> # ineligible dates are masked
  group_by(BENE_ID) |>
  mutate(has_new_oud = as.numeric(any(has_oud == 1))) |>
  arrange(oud_dt) |>
  summarise(has_new_oud = first(has_new_oud),
            oud_dt = first(oud_dt))

saveRDS(oud_cohort, file.path(save_dir, "cohort_has_new_oud.rds"))
