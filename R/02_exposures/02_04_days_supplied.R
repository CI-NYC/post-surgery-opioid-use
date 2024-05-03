# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(lubridate)
library(foreach)
library(doFuture)
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

days_supplied <- function(data) {
  dur <- 0
  current_int <- data$rx_int[1]
  for (i in 1:nrow(data)) {
    check <- intersect(current_int, data$rx_int[i + 1])
    if (is.na(check)) {
      # if they don't intersect, add the duration of the first interval
      dur <- dur + as.duration(current_int)
      current_int <- data$rx_int[i + 1]
    } else {
      # if they do intersect, then update current interval as the union
      current_int <- union(current_int, data$rx_int[i + 1])
    }
  }
  time_length(dur, "days")
}

plan(multisession, workers = 10)

opioids$days_supplied <- 
  foreach(x = opioids$data, 
          .combine = "c",
          .options.future = list(chunk.size = 1e4)) %dofuture% {
            days_supplied(x)
          }

plan(sequential)

opioids <- select(opioids, BENE_ID, days_supplied)

saveRDS(opioids, "/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/surgery_opioid_days_supplied.rds")


# I capped the rx_int at surgery dt + 15. so a lot of prescriptions starting on the 13th, 14th, or 15th day have a "shortened" days supply result