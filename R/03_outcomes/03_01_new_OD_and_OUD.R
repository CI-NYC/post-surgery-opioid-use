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

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds") |>
  select(BENE_ID, oud_hillary_dt, oud_poison_dt)

surgeries <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds") |>
  select(BENE_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT) |>
  left_join(cohort, by = "BENE_ID") |>
  as.data.table()


surgeries[, new_OUD := fifelse(oud_hillary_dt %within% interval(LINE_SRVC_END_DT, LINE_SRVC_END_DT %m+% years(2)), 1, 0)]
surgeries <- surgeries |> mutate(new_OUD = replace_na(new_OUD, 0))

surgeries[, new_OD := fifelse(oud_poison_dt %within% interval(LINE_SRVC_END_DT, LINE_SRVC_END_DT %m+% years(2)), 1, 0)]
surgeries <- surgeries |> mutate(new_OD = replace_na(new_OD, 0))

