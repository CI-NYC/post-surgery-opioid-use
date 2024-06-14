# -------------------------------------
# Script: 04_06_type_of_surgery
# Author: Anton Hung 2024-05-30
# Purpose:
# Notes:
# -------------------------------------
library(yaml)
library(dplyr)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

# surgery claims
variable = "Surgery"
codes <- read_yaml("/home/amh2389/medicaid/post_surgery_opioid_use/input/surgery_codes.yml")
codes <- c(names(codes[[variable]]$CPT),
           names(codes[[variable]]$ICD10))

surgery_type <- data.frame(
  PRCDR_CD = codes,
  surgery_major = c(rep(0,21), rep(1,length(codes)-21))
)

cohort$PRCDR_CD <- sapply(cohort$PRCDR_CD, first)

cohort <- cohort |>
  left_join(surgery_type, by="PRCDR_CD")

# expecting an 80 - 20 percent split between minor/major. But it's nearly 100% minor in my cohort

cohort <- cohort |>
  select("BENE_ID","surgery_major")
saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confound_major_surgery.rds")
