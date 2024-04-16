# -------------------------------------
# Script: continuous enroll
# Author: Anton Hung
# Purpose: identify all individuals who remain continuously enrolled for 6 months prior to surgery
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)

# read surgery claims
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims_with_opioids.rds")
setDT(claims)

dts_cohort <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdedts/nest_dts.rds")
setDT(dts_cohort)

dts_cohort <- dts_cohort[BENE_ID %in% claims$BENE_ID]
# small_cohort <- dts_cohort[1:1000,]
# small_claims <- claims[100001:101000,]

# Can do some preliminary filtering. Examples
# 1. removing dts_cohort if too few rows (< 3)
# 2....

# who to exclude based on continuous enrollment criteria

# tic()
claims$cohort_exclusion_noncontinuous <- (sapply(1:nrow(claims), function(i) {
  # find claims where claims$BENE_ID %in% small_cohort$BENE_ID
  enrollment_start <- claims[[i,"LINE_SRVC_BGN_DT"]] %m-% months(6)

  x <- dts_cohort[BENE_ID == claims[[i,"BENE_ID"]]]$data |>
    as.data.frame()

  continue = F
  for (j in 1:nrow(x)) {
    if (enrollment_start %within% interval(x[j,"ENRLMT_START_DT"], x[j,"ENRLMT_END_DT"])){
      continue = T
      # keeps track of j automatically (the row where our first enrollment period of interest begins)
      next
    }
  }

  # return a vector of dates or NA values
  if (continue) {
    # return(1)

    # end of row - enrollment_start  > 6
    if (x[j,"ENRLMT_END_DT"] - enrollment_start > months(6)) {
      return (0) # date
    } else if (nrow(x) > j) {
      # 3 things:
      # need a next row
      # need the next row to be continuous with the current row
      # need the next row's end date to be at least 6 months after enrollment_start 
      
      if (x[j+1,"ENRLMT_START_DT"] == x[j,"ENRLMT_END_DT"] + 1 &
          x[j+1,"ENRLMT_END_DT"] - enrollment_start > months(6)) {
        return (0)
      }
    }
  }
  return (1)
  
}))

# toc()

claims <- claims[, c("CLM_ID", "cohort_exclusion_noncontinuous")]
saveRDS(claims, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/cohort_exclusion_noncontinuous.rds")