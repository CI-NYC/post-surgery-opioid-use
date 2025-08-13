# READ ME -----------------------------------------------------------------
#
#       Author: Sarah Forrest
#      Created: 24 Aug 2023
#       edited: 27 Jan 2024 (Nick)
#  Last edited: 08 May 2024 (Anton Hung)

# 
#        Notes: Modified code originally written by Kat Hoffman and Nick
#               Williams in the 0x_mediator_dose_mme.R script
#        
#       Output: A mediator dataset with the the mean daily dose (MME) of 
#           opioids prescribed to a beneficiary within the mediator period, 
#           accounting for multiple prescriptions on the same day, for all 
#           beneficiaries in the analysis cohort
#
#
# -------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)
library(tictoc)
library(foreach)
library(doFuture)
library(furrr)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use"

# Read in cohort and dates
dts_cohorts <- readRDS(file.path(save_dir, "intermediate/first_surgeries.rds"))
setDT(dts_cohorts)
setkey(dts_cohorts, BENE_ID)

all_opioids_clean <- readRDS(file.path(save_dir, "opioid_data/opioids_for_surgery_cleaned.rds"))
setDT(all_opioids_clean)
setkey(all_opioids_clean, BENE_ID)
# all_opioids_clean <- all_opioids_clean[, .(BENE_ID, NDC, opioid, mme_strength_per_day, rx_start_dt, rx_end_dt)]


# Calculate max daily dose -----------------------------------------------------

# Group by beneficiary and create a list column containing each beneficiairy's data
opioids <- all_opioids_clean[, list(data = list(data.table(.SD))), by = BENE_ID]
opioids <- opioids[1:100000]
# Create function
calculate_mean_daily_dose <- function(data) {
  to_modify <- copy(data)
  
  # to_modify[, c("rx_start_dt", "rx_end_dt") := lapply(.SD, as.Date), 
  #           .SDcols = c("rx_start_dt", "rx_end_dt")]
  
  # Calculate the date limit based on washout_cal_end_dt + 182 days
  washout_date_limit <- to_modify$discharge_dt + days(14)
  
  long <- to_modify[, .(date = seq(rx_start_dt, rx_end_dt - 1, by = "1 day"), 
                        NDC, opioid, mme_strength_per_day,rx_start_dt,discharge_dt), by = .(seq_len(nrow(data)))
  ][date <= washout_date_limit, ]  # Filter rows based on date limit
  
  long[, .(observation_count=.N, 
           same_drug=length(opioid) > 1 & all(opioid==first(opioid)),
           oxycodone=length(opioid) > 1 & any(opioid=="oxycodone") & !all(opioid=="oxycodone"),
           tramadol=length(opioid) > 1 & any(opioid=="tramadol") & !all(opioid=="tramadol"),
           hydrocodone=length(opioid) > 1 & any(opioid=="hydrocodone") & !all(opioid=="hydrocodone"),
           all_oxycodone=length(opioid) > 1 & any(opioid=="oxycodone"),
           all_tramadol=length(opioid) > 1 & any(opioid=="tramadol"),
           all_hydrocodone=length(opioid) > 1 & any(opioid=="hydrocodone"),
           total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE),
           rx_start_dt,
           discharge_dt
           ), by = .(date)
  ][, .(overlapping_opioids = any(observation_count>1),
        same_drug = any(same_drug),
        oxycodone = any(oxycodone),
        tramadol = any(tramadol),
        hydrocodone = any(hydrocodone),
        all_oxycodone = any(all_oxycodone),
        all_tramadol = any(all_tramadol),
        all_hydrocodone = any(all_hydrocodone),
        max_daily_dose_mme = max(total_mme_strength),
        initial_yn = total_mme_strength[which.max(total_mme_strength)] == first(total_mme_strength)
        )]
}

plan(multisession, workers = 10)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- calculate_mean_daily_dose(data)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

# out <- out |>
#   mutate(mediator_mean_daily_dose_mme = pmin(mediator_mean_daily_dose_mme, quantile(mediator_mean_daily_dose_mme, 0.99)))

# Save final dataset -----------------------------------------------------------

saveRDS(out, "/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/overlapping_opioids.rds")


# Additional capping based on eyeballing the histogram
# out <- readRDS(file.path(save_dir, "opioid_data/surgery_max_daily_dose_mme.rds"))

# hist(out$mean_daily_dose_mme,xlim=c(100,200), ylim=c(0,5000), breaks = 100)
# hist(out$mean_daily_dose_mme,xlim=c(110,125), ylim=c(0,5000), breaks = 100)
#
# 99 percentile occurs at 115, so we decide to cap at 115

# out <- out |>
#   mutate(max_daily_dose_mme = ifelse(max_daily_dose_mme > 125, 125, max_daily_dose_mme))

# hist(out$mean_daily_dose_mme)

# saveRDS(out, file.path(save_dir, "opioid_data/surgery_max_daily_dose_mme_truncated.rds"))
