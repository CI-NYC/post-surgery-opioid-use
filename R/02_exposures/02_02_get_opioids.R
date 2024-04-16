# -------------------------------------
# Script: 02_get_opioids
# Author: Anton Hung
# Purpose: get opioid-related exposure variables
# Notes: 
# -------------------------------------
library(data.table)
library(lubridate)
library(dplyr)
# need:
# mme, days supply, continuous days

# relevant vars:
# otl:
# 1. diff between end dt and bgn dt (days supply)

# rxl
# 1. days supply

claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims_with_opioids.rds")
setDT(claims)

########## LOADING OPIOIDS
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




####### COMBINING OTL and RXL into one dataframe
opioids <- otl |> 
  mutate(DAYS_SUPPLY = as.numeric(LINE_SRVC_END_DT - LINE_SRVC_BGN_DT)) |> 
  mutate(otl_rxl = "otl") |>
  select(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, NDC, NDC_QTY, DAYS_SUPPLY, TOS_CD, otl_rxl) |>
  rename(RX_FILL_DT = LINE_SRVC_BGN_DT) |>
  as_tibble() |> 
    
  bind_rows({
    rxl |> 
      mutate(otl_rxl = "rxl") |> 
      # select(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, NDC, NDC_QTY, DAYS_SUPPLY, TOS_CD, otl_rxl) |>
      as_tibble()
  }) |> as.data.table()


####### LOOKUP which OTL and RXL rows are within the perioperative surgery period

# creating an index column to keep track of opioids
opioids$index <- 1:nrow(opioids)

# Initialize an empty set for eligible opioids
eligible_opioids <- c()

# Apply a custom function to each row in claims
for (i in 1:nrow(claims)) {
  # Find matching rows in opioids
  matching_opioids <- opioids[BENE_ID == claims[i, "BENE_ID"], ]
  # Filter rows based on date interval
  eligible_rows <- matching_opioids[RX_FILL_DT %within% 
                                      interval(claims[[i, "LINE_SRVC_BGN_DT"]] %m-% months(1), 
                                               claims[[i, "LINE_SRVC_END_DT"]] %m+% days(14)), index]
  
  # Collect index IDs
  eligible_opioids <- union(eligible_opioids, eligible_rows)
}

eligible_opioids <- unique(eligible_opioids)


########## save opioids
opioids <- opioids[index %in% eligible_opioids]

saveRDS(opioids, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/eligible_opioids.rds")
