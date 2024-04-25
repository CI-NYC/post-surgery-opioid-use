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

opioids <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/eligible_opioids.rds") |>
  mutate(rx_int = interval(LINE_SRVC_BGN_DT, LINE_SRVC_END_DT)) |>
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
  dur
}

plan(multisession, workers = 50)

opioids$days_supplied <- 
  foreach(x = opioids$data, 
          .combine = "c",
          .options.future = list(chunk.size = 1e4)) %dofuture% {
            days_supplied(x)
          }

plan(sequential)

opioids <- select(cohort, BENE_ID) |> 
  left_join(select(opioids, -data)) |> 
  mutate(days_supplied = replace_na(days_supplied, 0))

saveRDS(opioids, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/cohort_days_supplied.rds")





