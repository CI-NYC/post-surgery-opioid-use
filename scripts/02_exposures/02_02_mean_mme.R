# -------------------------------------
# Script: 03_mean_mme.R
# Author: Nick Williams
# Purpose: Calculate mean daily dose (MME) for opioids prescribed during the exposure period
# Notes: Modified from https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/02_06mo/10_mme/02_treatment_max_daily_dose_mme.R
# -------------------------------------

library(tidyverse)
library(readxl)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)

source("~/medicaid/post_surgery_opioid_use/R/helpers.R")

# load cohort and opioid data
cohort <- readRDS(file.path(drv_root, "intermediate/first_surgeries.rds"))

opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  right_join(cohort) |>
  mutate(rx_end_dt = as.Date(pmin(rx_end_dt, discharge_dt + days(14))))

setDT(opioids)
setkey(opioids, BENE_ID)

opioids <- opioids[, .(BENE_ID, 
                       rx_start_dt, rx_end_dt, NDC, opioid, mme_strength_per_day)]
  

num_opioids <- opioids |>
  group_by(BENE_ID) |>
  summarize(num_opioids = n())

saveRDS(num_opioids, file.path(drv_root, "treatment/num_opioids.rds"))

# Calculate mean daily dose -----------------------------------------------------

opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]

calculate_mean_daily_dose <- function(data) {
  to_modify <- copy(data)
  
  to_modify[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), NDC, opioid, mme_strength_per_day), 
            by = .(seq_len(nrow(data)))
  ][, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE)), 
    by = .(date)
  ][, .(exposure_mean_daily_dose_mme = mean(total_mme_strength))]
}

plan(multisession, workers = 5)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 4e4)) %dofuture% {
                 out <- calculate_mean_daily_dose(data)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

testthat::test_that(
  "All observations have a mean daily MME",
  testthat::expect_false(any(is.na(out$exposure_mean_daily_dose_mme)))
)

write_data(out, "exposure_mean_daily_dose_mme.fst", file.path(drv_root, "treatment"))



out <- load_data("exposure_mean_daily_dose_mme.fst", file.path(drv_root, "treatment"))

out <- out |>
  mutate(exposure_mean_daily_dose_mme = ifelse(exposure_mean_daily_dose_mme > 115, 115, exposure_mean_daily_dose_mme))

write_data(out, "exposure_mean_daily_dose_mme.fst", file.path(drv_root, "treatment"))
