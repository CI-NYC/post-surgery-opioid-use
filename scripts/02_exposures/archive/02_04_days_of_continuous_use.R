# -------------------------------------
# Script: days_of_continuous_use
# Author: Anton Hung
# Purpose: similar to 02_01_days_supplied, but this one stores lengths of all continuous periods of
#          opioid use into a vector
# Notes:
# -------------------------------------


library(dplyr)
library(lubridate)
library(tidyverse)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
  
opioids <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/opioids_for_surgery.rds") |>
  right_join(cohort[, c("CLM_ID", "surgery_dt", "discharge_dt")], by="CLM_ID") |>
  mutate(DAYS_SUPPLY = replace_na(DAYS_SUPPLY, 1),
         rx_int = interval(RX_FILL_DT, pmin(RX_FILL_DT %m+% days(DAYS_SUPPLY),
                                            discharge_dt %m+% days(15)))) |>
  select(BENE_ID, rx_int) |>
  group_by(BENE_ID) |> 
  arrange(BENE_ID, int_start(rx_int)) |> 
  nest()
                     
                     
days_continuous <- function(data) {
  out <- c()
  current_int <- data$rx_int[1]
  for (i in 1:nrow(data)) {
    check <- intersect(current_int, data$rx_int[i + 1])
    if (!is.na(check)) {
      current_int <- union(current_int, data$rx_int[i + 1])
     
    } else {
      out <- c(out, time_length(current_int, "days"))
      current_int <- data$rx_int[i + 1]
    }
  }
  out
}

opioids$days_of_continuous_use <- sapply(opioids$data, days_continuous)

opioids <- opioids |>
  select(BENE_ID, days_of_continuous_use)

saveRDS(opioids, "/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/surgery_opioids_days_continuous.rds")

# opioids$max_days <- sapply(opioids$days_of_continuous_use, max)


