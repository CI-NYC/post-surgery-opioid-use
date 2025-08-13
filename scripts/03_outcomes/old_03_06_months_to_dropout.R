# -------------------------------------
# Script: months_to_dropout
# Author: Anton Hung
# Purpose: 
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)
library(survival)
library(ggplot2)

# read surgery claims
cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
setDT(cohort)
cohort <- cohort[,.(BENE_ID, followup_start_dt)]


######## ALTERNATIVE METHOD - in progress

library(arrow)

src_root <- "/mnt/processed-data/disability"

# Read in DTS 
files <- paste0(list.files(src_root, pattern = "TAFDEDTS", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
dts <- open_dataset(file.path(src_root, parquet_files))

dts <- dts |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  select(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT) |>
  collect()

filtered_dts <- cohort |>
  left_join(dts, by= "BENE_ID") |>
  filter(followup_start_dt <= ENRLMT_END_DT)

# Convert date columns to numeric (days since a reference date)
ranges <- filtered_dts %>%
  mutate(
    start_num = as.numeric(ENRLMT_START_DT),
    end_num = as.numeric(ENRLMT_END_DT)
  )

# Arrange by start date within each ID group
collapsed_ranges <- ranges %>%
  group_by(BENE_ID) %>%
  arrange(start_num) %>%
  mutate(prev_end_num = lag(end_num, default = first(end_num))) %>%
  filter(start_num <= prev_end_num + 92) %>%
  summarise(
    followup_start_dt = as.Date(first(followup_start_dt)),
    enrolled_until = as.Date(max(ENRLMT_END_DT))
  ) |> 
  mutate(months_to_dropout = time_length(enrolled_until - followup_start_dt, "months"))

saveRDS(collapsed_ranges, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")
