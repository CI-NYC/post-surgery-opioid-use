# -------------------------------------
# Script: 02_cohort_mme_join.R
# Author: Nick Williams
# Purpose: Calculate MME/strength per day for opioids in exposure period
# Notes: Modified from: 
#   - https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/01_00_treatment_dose_mme.R
#   - https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/01_01_treatment_dose_mme.R
# -------------------------------------

library(tidyverse)
library(fst)
library(lubridate)
library(data.table)
library(arrow)

source("~/medicaid/post_surgery_opioid_use/R/helpers.R")

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# load cohort
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")
setDT(claims)

mme <- readRDS("~/medicaid/post_surgery_opioid_use/input/opioids_mme.rds")

bup_list <- read_fst("~/medicaid/post_surgery_opioid_use/input/bup_list.fst")

rxl_opioids <-
  rxl |>
  filter(NDC %in% mme$NDC) |>
  select(BENE_ID, RX_FILL_DT, contains("ndc"), DAYS_SUPPLY) |>
  collect() |>
  left_join(mme)

rxl_opioids <- left_join(claims, rxl_opioids)

rxl_opioids <- 
  rxl_opioids |> 
  filter((RX_FILL_DT >= surgery_dt %m-% days(30)) & 
           (RX_FILL_DT <= discharge_dt + days(14)))


# calculate strength per day in Milligram Morphine Equivalent (MME) units
# no caps on number of pills, days supply, and pills per day
rxl_opioids <-
  rxl_opioids |>
  drop_na(BENE_ID) |>
  mutate(number_pills = case_when(!is.na(NDC_QTY) ~ abs(NDC_QTY),
                                  TRUE ~ 1),
         days_supply = case_when(!is.na(DAYS_SUPPLY) ~ DAYS_SUPPLY,
                                 TRUE ~ 1), # best assumption we can make if missing a days supply var
         pills_per_day = number_pills / days_supply,
         strength = parse_number(numeratorValue),
         strength_per_day = strength * pills_per_day,
         mme_strength_per_day = strength_per_day * conversion#, 
         # mme_strength_per_day = pmin(mme_strength_per_day, quantile(mme_strength_per_day, 0.99))
  )

# keep only relevant vars for RXL opioids
rxl_opioids <-
  rxl_opioids |>
  select(BENE_ID,
         CLM_ID,
         # pain_diagnosis_dt, 
         # last_treatment_dt,
         surgery_dt,
         discharge_dt,
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
  mutate(rx_end_dt = rx_start_dt + days_supply - 1) |>
  arrange(BENE_ID, rx_start_dt, opioid)


opioids <- rxl_opioids |>
  unique() |> 
  mutate(days_supply = replace_na(days_supply, 1))

# remove opioids (buprenorphine) if they belong to our list of buprenorphine codes that are specifically used to treat MOUD
opioids_wo_moud_bup <- opioids |>
  filter(!NDC %in% bup_list$ndc)

write_data(opioids_wo_moud_bup, "exposure_period_opioids.fst", file.path(drv_root, "treatment"))


cohort <- claims |>
  mutate(cohort_exclusion_eligible_opioid = as.numeric(!CLM_ID %in% opioids_wo_moud_bup$CLM_ID)) |>
  select(BENE_ID, CLM_ID, cohort_exclusion_eligible_opioid)

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_eligible_opioid.rds")
