# -------------------------------------
# Script: exclusion_opioids
# Author: Anton Hung
# Purpose: exclude beneficiaries based on opioid prescription criteria
# Notes:
# -------------------------------------
library(lubridate)
library(dplyr)
library(data.table)
library(arrow)
library(yaml)
# criteria:
# opioid prescribed during 1 month before surgery to 14 days after discharge
# NO opioids prescribed during the 6 month washout prior to surgery

# intermediate claims data
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")
setDT(claims)

# otl and rxl data for finding opioid claims
src_root <- "/mnt/processed-data/disability"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# Read in RXL (pharmacy line)
files <- paste0(list.files(src_root, pattern = "TAFRXL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxl <- open_dataset(file.path(src_root, parquet_files))

# Read in opioid pain list
op <- readRDS(file.path("/mnt/general-data/disability/mediation_unsafe_pain_mgmt", "mediation_unsafe_pain_mgmt_opioid_pain_ndc.rds"))


# OTL ---------------------------------------------------------------------

# Filter OTL to opioid pain NDC
otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY", "TOS_CD")

otl <- select(otl, all_of(otl_vars)) |> 
  filter(NDC %in% op$NDC) |>
  collect() |> 
  filter(BENE_ID %in% claims$BENE_ID) |>
  filter(!TOS_CD == "001") |>
  as.data.table()

otl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                  LINE_SRVC_END_DT, 
                                  LINE_SRVC_BGN_DT)]

# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "CLM_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY", "TOS_CD")

rxl <- select(rxl, all_of(rxl_vars)) |> 
  filter(NDC %in% op$NDC) |>
  collect() |> 
  filter(BENE_ID %in% claims$BENE_ID) |>
  as.data.table()


# returning whether or not to keep a surgery observation based on surgery dates and opioids dates
opioid_exclude <- function(id, surgery_dt, discharge_dt, opioid_otl, opioid_rxl){
  opioid_dates <- c(opioid_otl[BENE_ID == id, LINE_SRVC_BGN_DT], 
                    opioid_rxl[BENE_ID == id, RX_FILL_DT])
  
  return (any(opioid_dates %within% interval(surgery_dt %m-% months(6), surgery_dt %m-% months(1))))
}

surgeries_to_exclude <- sapply(1:nrow(claims), function(i) {
  opioid_exclude(claims$BENE_ID[i], claims$LINE_SRVC_BGN_DT[i], claims$LINE_SRVC_END_DT[i], otl, rxl)
})


cohort_exclusion_opioids <- claims[, "CLM_ID"] |>
  mutate(cohort_exclusion_opioids = as.numeric(surgeries_to_exclude))

saveRDS(cohort_exclusion_opioids, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/cohort_exclusion_opioids.rds")


# tmp <- (otl[TOS_CD=="001"]) |>
#   mutate(new = difftime(LINE_SRVC_END_DT,LINE_SRVC_BGN_DT))

# confirmed they are all single-day opioids

