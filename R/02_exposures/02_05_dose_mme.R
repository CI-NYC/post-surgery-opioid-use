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

opioids <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/eligible_opioids.rds")

opioids <- opioids |>
  mutate(number_pills = case_when(!is.na(NDC_QTY) ~ abs(NDC_QTY),
                                  TRUE ~ 1),
         days_supply = as.numeric(LINE_SRVC_END_DT - LINE_SRVC_BGN_DT),
         pills_per_day = number_pills / days_supply,
         strength = parse_number(numeratorValue),
         strength_per_day = strength * pills_per_day,
         mme_strength_per_day = strength_per_day * conversion, 
         mme_strength_per_day = pmin(mme_strength_per_day, quantile(mme_strength_per_day, 0.99)))


td <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"


