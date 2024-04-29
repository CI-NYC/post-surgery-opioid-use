# -------------------------------------
# Script: new_OD_and_OUD
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
library(lubridate)
library(tidyverse)

all_poison <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_poison_dts.rds")

all_hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/all_hillary_dts.rds")

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds") |>
  select(BENE_ID, surgery_dt, discharge_dt) |>
  as.data.table()


# POISON
poison_cohort <- left_join(cohort, all_poison, by="BENE_ID")

poison_cohort[, has_poison := 
         as.numeric(oud_poison_dt > discharge_dt)] 

poison_cohort <- poison_cohort |>
  mutate(has_poison = case_when(is.na(has_poison) ~ 0, TRUE ~ has_poison), # converting NA to 0
         oud_poison_dt = case_when(has_poison == 0 ~ NA, TRUE ~ oud_poison_dt)) |> # ineligible dates are masked
  group_by(BENE_ID) |>
  mutate(has_new_poison = as.numeric(any(has_poison == 1))) |>
  arrange(oud_poison_dt) |>
  slice(1) |>
  ungroup() |>
  select(BENE_ID, has_new_poison, oud_poison_dt)

saveRDS(poison_cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_poison.rds")

# HILLARY
hillary_cohort <- left_join(cohort, all_hillary, by="BENE_ID")

hillary_cohort[, has_hillary := 
                as.numeric(oud_hillary_dt > discharge_dt)] 

hillary_cohort <- hillary_cohort |>
  mutate(has_hillary = case_when(is.na(has_hillary) ~ 0, TRUE ~ has_hillary), # converting NA to 0
         oud_hillary_dt = case_when(has_hillary == 0 ~ NA, TRUE ~ oud_hillary_dt)) |> # ineligible dates are masked  
  group_by(BENE_ID) |>
  mutate(has_new_hillary = as.numeric(any(has_hillary == 1))) |>
  arrange(oud_hillary_dt) |>
  slice(1) |>
  ungroup() |>
  select(BENE_ID, has_new_hillary, oud_hillary_dt)

saveRDS(hillary_cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")
