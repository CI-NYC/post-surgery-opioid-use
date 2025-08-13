# -------------------------------------
# Script: new_OD_and_OUD
# Author: Anton Hung 2024-05-10
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
library(lubridate)
library(tidyverse)

load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/intermediate"
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/outcomes"

all_poison <- readRDS(file.path(load_dir, "all_poison_dts.rds"))

all_hillary <- readRDS(file.path(load_dir, "all_hillary_dts.rds"))

cohort <- readRDS(file.path(load_dir, "first_surgeries.rds")) |>
  select(BENE_ID, followup_start_dt = discharge_dt) |>
  as.data.table()


# POISON
poison_cohort <- left_join(cohort, all_poison, by="BENE_ID")

poison_cohort[, has_poison := 
                as.numeric(oud_poison_dt %within% interval(followup_start_dt, followup_start_dt %m+% years(2)))] 

poison_cohort <- poison_cohort |>
  mutate(has_poison = fifelse(is.na(has_poison), 0, has_poison), # converting NA to 0
         oud_poison_dt = fifelse(has_poison == 0, NA, oud_poison_dt)) |> # ineligible dates are masked
  group_by(BENE_ID) |>
  mutate(has_new_poison = as.numeric(any(has_poison == 1))) |>
  arrange(oud_poison_dt) |>
  summarise(has_new_poison = first(has_new_poison),
            oud_poison_dt = first(oud_poison_dt))

saveRDS(poison_cohort, file.path(save_dir, "cohort_has_new_poison.rds"))

# HILLARY
hillary_cohort <- left_join(cohort, all_hillary, by="BENE_ID")

hillary_cohort[, has_hillary := 
                 as.numeric(oud_hillary_dt %within% interval(followup_start_dt, followup_start_dt %m+% years(2)))] 

hillary_cohort <- hillary_cohort |>
  mutate(has_hillary = fifelse(is.na(has_hillary), 0, has_hillary), # converting NA to 0
         oud_hillary_dt = fifelse(has_hillary == 0, NA, oud_hillary_dt)) |> # ineligible dates are masked
  group_by(BENE_ID) |>
  mutate(has_new_hillary = as.numeric(any(has_hillary == 1))) |>
  arrange(oud_hillary_dt) |>
  summarise(has_new_hillary = first(has_new_hillary),
            oud_hillary_dt = first(oud_hillary_dt))

saveRDS(hillary_cohort, file.path(save_dir, "cohort_has_new_hillary.rds"))
