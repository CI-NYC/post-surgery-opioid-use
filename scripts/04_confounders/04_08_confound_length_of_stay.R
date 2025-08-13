# -------------------------------------
# Script: length of stay
# Author: Anton Hung
# Date: July 22 2024
# Purpose: creating a confounder for length of stay at the hospital
# Notes:
# -------------------------------------
library(dplyr)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
# cohort <- cohort[1:10000,]

length_of_stay <- cohort |>
  mutate(length_of_stay = as.numeric(discharge_dt - surgery_dt + 1)) |>
  select(BENE_ID, length_of_stay)
  
saveRDS(length_of_stay, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/length_of_stay.rds")





