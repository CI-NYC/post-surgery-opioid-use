# -------------------------------------
# Script: more_filtering_surgeries
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(dplyr)
library(lubridate)
# removing duplicate observations (not sure what to do with them right now)
# defined as same BENE_ID, same BGN_DT and same END_DT. They may have different claim IDs and procedure codes because they are different procedures, but we don't care in this analysis?

surgeries <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/cleaned_surgeries.rds")
setDT(surgeries)

# surgeries <- surgeries[!duplicated(surgeries[, c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT")]), ] |>
  # select(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, LINE_PRCDR_CD_SYS, LINE_PRCDR_CD)

# this reduces the size of the data frame from 232183 to 144393

# Still a small number of weird cases. Example:
#            BENE_ID          CLM_ID LINE_SRVC_BGN_DT LINE_SRVC_END_DT LINE_PRCDR_CD_SYS LINE_PRCDR_CD
# <char>          <char>           <Date>           <Date>            <char>        <char>
#   1: HHHHHHBHCHnn4Hn HHHH4HeB47ABd7B       2016-08-06       2016-08-10                01         47563
# 2: HHHHHHBHCHnn4Hn HHHH4HeBdBeCABH       2016-08-10       2016-08-10                01         47563

# saveRDS(surgeries, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/filtered_surgeries.rds")



surgeries <- surgeries |>
  group_by(BENE_ID) |>
  slice(1)


saveRDS(surgeries, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
