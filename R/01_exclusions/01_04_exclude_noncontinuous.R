# -------------------------------------
# Script: continuous enroll
# Author: Anton Hung
# Purpose: identify all individuals who remain continuously enrolled for 6 months prior to surgery
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)

# read surgery claims
claims <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")
setDT(claims)
# claims <- claims[1:1000]

# dts_cohort <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdedts/nest_dts.rds")
# setDT(dts_cohort)

# dts_cohort <- dts_cohort[BENE_ID %in% claims$BENE_ID]
# claims <- claims[BENE_ID %in% dts_cohort$BENE_ID]

# small_cohort <- dts_cohort[1:1000,]
# small_claims <- claims[100001:101000,]

# Can do some preliminary filtering. Examples
# 1. removing dts_cohort if too few rows (< 3)
# 2....

# who to exclude based on continuous enrollment criteria

# tic()
# claims$cohort_exclusion_noncontinuous <- (sapply(1:nrow(claims), function(i) {
#   if (i %in% c(281671, 281672, 281673, 281674)){ # for some reason, these rows belong to a beneficiary: HHHHHHBdkH77Ad7, who does not exist in nest_dts.rds
#     return (1)
#   }
#   # find claims where claims$BENE_ID %in% small_cohort$BENE_ID
#   enrollment_start <- claims[[i,"washout_start_dt"]]
# 
#   x <- dts_cohort[BENE_ID == claims[[i,"BENE_ID"]]]$data |>
#     as.data.frame()
# 
#   continue = F
#   for (j in 1:nrow(x)) {
#     if (enrollment_start %within% interval(x[[j,"ENRLMT_START_DT"]], x[[j,"ENRLMT_END_DT"]])){
#       continue = T
#       # keeps track of j automatically (the row where our first enrollment period of interest begins)
#       next
#     }
#   }
# 
#   # return a vector of dates or NA values
#   if (continue) {
#     # return(1)
# 
#     # end of row - enrollment_start  > 6
#     if (x[[j,"ENRLMT_END_DT"]] - enrollment_start > months(6)) {
#       return (0) # date
#     } else if (nrow(x) > j) {
#       # 3 things:
#       # need a next row
#       # need the next row to be continuous with the current row
#       # need the next row's end date to be at least 6 months after enrollment_start 
#       
#       if (x[[j+1,"ENRLMT_START_DT"]] == x[[j,"ENRLMT_END_DT"]] + 1 &
#           x[[j+1,"ENRLMT_END_DT"]] - enrollment_start > months(6)) {
#         return (0)
#       }
#     }
#   }
#   return (1)
#   
# }))




# claims$cohort_exclusion_noncontinuous <- (sapply(1:nrow(claims), function(i) {
# 
#   # find claims where claims$BENE_ID %in% small_cohort$BENE_ID
#   surgery_dt <- claims[[i,"surgery_dt"]]
#   data <- dts_cohort[BENE_ID == claims[[i,"BENE_ID"]]]$data[[1]] |>
#     group_by(ENRLMT_START_DT) |>
#     mutate(ENRLMT_END_DT = max(ENRLMT_END_DT)) |>
#     slice(1) |>
#     ungroup() |> 
#     mutate(ascending = ENRLMT_END_DT >= lag(ENRLMT_END_DT, default = first(ENRLMT_END_DT))) |>
#     filter(ascending == T)
#   
#   enrollment_date <- data$ENRLMT_START_DT
#   enrollment_end <- data$ENRLMT_END_DT
#   last_enrollment_end <- lag(enrollment_end)
#   
#   q1 <- surgery_dt %within% interval(enrollment_date, enrollment_end)
#   
#   q2 <- time_length(surgery_dt - enrollment_date, "days") >= 182
#   
#   if (any(q1 & q2)) return(0)
#   
#   # QUESTION 3: is the previous row continuous with the current row
#   end <- length(enrollment_date)
#   q3 <- (last_enrollment_end[1:end] + days(1)) == enrollment_date[1:end]
#   
#   # QUESTION 4: is the previous row continuous with the current row for at least 6 months?
#   # q4 <- (interval(lag(enrollment_date), enrollment_end)) |>
#   #   as.period() |>
#   #   month() >= 6
#   q4 <- time_length(surgery_dt - lag(enrollment_date), "days") >= 182
#   
#   
#   if (any(q1 & q3 & q4, na.rm = TRUE)) {
#     return(0)
#   }
#   return(1)
# }))
# 
# claims <- claims[, c("BENE_ID", "CLM_ID", "cohort_exclusion_noncontinuous")]
# saveRDS(claims, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_noncontinuous.rds")







############# ANOTHER METHOD - in progress
library(arrow)
src_root <- "/mnt/processed-data/disability"

# Read in DTS 
files <- paste0(list.files(src_root, pattern = "TAFDEDTS", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
dts <- open_dataset(file.path(src_root, parquet_files))

dts <- dts |>
  filter(BENE_ID %in% claims$BENE_ID) |>
  collect()

filtered_claims <- claims |>
  left_join(dts, by= "BENE_ID", relationship = "many-to-many") |>
  filter(washout_start_dt %within% interval(ENRLMT_START_DT, ENRLMT_END_DT) |
           (discharge_dt + days(14)) %within% interval(ENRLMT_START_DT, ENRLMT_END_DT))



# Convert date columns to numeric (days since a reference date)
ranges <- filtered_claims %>%
  mutate(
    start_num = as.numeric(ENRLMT_START_DT),
    end_num = as.numeric(ENRLMT_END_DT)
  )

# Arrange by start date within each ID group
collapsed_ranges <- ranges %>%
  group_by(CLM_ID) %>%
  arrange(start_num) %>%
  mutate(prev_end_num = lag(end_num, default = first(end_num))) %>%
  filter(start_num <= prev_end_num + 1) %>%
  summarise(
    date_start = as.Date(first(ENRLMT_START_DT)),
    date_end = as.Date(max(ENRLMT_END_DT))
  )


claims <- claims |>
  left_join(collapsed_ranges) |>
  mutate(cohort_exclusion_noncontinuous = case_when(
    washout_start_dt %within% interval(date_start, date_end) & 
      (discharge_dt + days(14)) %within% interval(date_start, date_end) ~ 0, TRUE ~ 1))


claims <- claims[, c("BENE_ID", "CLM_ID", "cohort_exclusion_noncontinuous")]
saveRDS(claims, "/mnt/general-data/disability/post_surgery_opioid_use/exclusion/cohort_exclusion_noncontinuous.rds")
