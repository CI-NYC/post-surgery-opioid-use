# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(tidyverse)
# library(janitor)
# library(readxl)
library(arrow)
library(lubridate)
library(data.table)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
setDT(cohort)

mme <- readRDS(file.path("/mnt/general-data/disability/mediation_unsafe_pain_mgmt", "opioids_mme.rds"))

opioids <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/opioids_for_surgery.rds") |>
  filter(CLM_ID %in% cohort$CLM_ID) |>
  left_join(mme)


# calculate strength per day in Milligram Morphine Equivalent (MME) units
# no caps on number of pills, days supply, and pills per day
opioids <-
  opioids |>
  drop_na(BENE_ID) |>
  mutate(number_pills = case_when(!is.na(NDC_QTY) ~ abs(NDC_QTY),
                                  TRUE ~ 1),
         days_supply = case_when(!is.na(DAYS_SUPPLY) ~ DAYS_SUPPLY,
                                 TRUE ~ 1), # best assumption we can make if missing a days supply var
         pills_per_day = number_pills / days_supply,
         strength = parse_number(numeratorValue),
         strength_per_day = strength * pills_per_day,
         mme_strength_per_day = strength_per_day * conversion, 
         mme_strength_per_day = pmin(mme_strength_per_day, quantile(mme_strength_per_day, 0.99)))


# keep only relevant vars for RXL opioids
opioids <-
  opioids |>
  select(BENE_ID,
         opioid,
         NDC,
         dose_form,
         days_supply,
         pills_per_day,
         strength,
         strength_per_day,
         mme_strength_per_day,
         days_supply,
         rx_start_dt = RX_FILL_DT) |>
  mutate(rx_end_dt = rx_start_dt + days_supply) |>
  arrange(BENE_ID, rx_start_dt, opioid)

opioids <- left_join(opioids, cohort[, .(BENE_ID, surgery_dt, discharge_dt)])

saveRDS(opioids, file.path("/mnt/general-data/disability/post_surgery_opioid_use/opioid_data", "opioids_for_surgery_cleaned.rds"))

