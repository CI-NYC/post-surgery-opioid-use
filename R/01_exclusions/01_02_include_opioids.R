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

# # Read in OTL (Other services line) 
# files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
# parquet_files <- grep("\\.parquet$", files, value = TRUE)
# otl <- open_dataset(file.path(src_root, parquet_files))

# Read in RXL (pharmacy line)
files <- paste0(list.files(src_root, pattern = "TAFRXL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxl <- open_dataset(file.path(src_root, parquet_files))

# Read in opioid pain list
op <- readRDS(file.path("/mnt/general-data/disability/mediation_unsafe_pain_mgmt", "mediation_unsafe_pain_mgmt_opioid_pain_ndc.rds"))


# OTL ---------------------------------------------------------------------

# # Filter OTL to opioid pain NDC
# otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY", "TOS_CD")
# 
# otl <- select(otl, all_of(otl_vars)) |> 
#   filter(NDC %in% op$NDC) |>
#   collect() |> 
#   filter(BENE_ID %in% claims$BENE_ID) |>
#   filter(!TOS_CD == "001") |>
#   select(-"TOS_CD") |>
#   as.data.table()
# 
# otl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
#                                   LINE_SRVC_END_DT, 
#                                   LINE_SRVC_BGN_DT)]

# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "CLM_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
  filter(NDC %in% op$NDC) |>
  collect() |> 
  filter(BENE_ID %in% claims$BENE_ID) |>
  rename(OPIOID_ID = CLM_ID) |>
  as.data.table()


# # returning whether or not to keep a surgery observation based on surgery dates and opioids dates
# opioid_exclude <- function(id, surgery_dt, discharge_dt, opioid_otl, opioid_rxl){
#   opioid_dates <- c(opioid_otl[BENE_ID == id, LINE_SRVC_BGN_DT], 
#                     opioid_rxl[BENE_ID == id, RX_FILL_DT])
#   
#   return (any(opioid_dates %within% interval(surgery_dt %m-% months(1), discharge_dt + days(14))))
# }
# 
# surgeries_to_keep <- sapply(1:nrow(claims), function(i) {
#   opioid_exclude(claims$BENE_ID[i], claims$LINE_SRVC_BGN_DT[i], claims$LINE_SRVC_END_DT[i], otl, rxl)
# })


claims_rxl_merged <- left_join(claims, rxl, by="BENE_ID", relationship = "many-to-many")

claims_rxl_merged[, eligible_opioid := 
                    as.numeric(RX_FILL_DT %within% interval(surgery_dt %m-% days(30),
                                                            discharge_dt %m+% days(14)))] 

opioids_for_surgery <- claims_rxl_merged[eligible_opioid==1, .(BENE_ID, 
                                                               CLM_ID, 
                                                               OPIOID_ID, 
                                                               RX_FILL_DT, 
                                                               NDC, 
                                                               NDC_QTY, 
                                                               DAYS_SUPPLY)]

saveRDS(opioids_for_surgery, "/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/opioids_for_surgery.rds")


# surgery_claims_with_opioids <- claims_rxl_merged |> 
#   select(BENE_ID, CLM_ID, washout_start_dt, surgery_dt, discharge_dt, LINE_PRCDR_CD_SYS, LINE_PRCDR_CD) |>
#   distinct()

cohort_exclusion_eligible_opioid <- claims_rxl_merged |>
  mutate(eligible_opioid = case_when(is.na(eligible_opioid) ~ 0, TRUE ~ eligible_opioid)) |>
  group_by(CLM_ID) |>
  mutate(cohort_exclusion_eligible_opioid = as.numeric(!any(eligible_opioid == 1))) |>
  ungroup() |>
  select(BENE_ID, CLM_ID, cohort_exclusion_eligible_opioid) |>
  distinct()

saveRDS(cohort_exclusion_eligible_opioid, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_eligible_opioid.rds")




