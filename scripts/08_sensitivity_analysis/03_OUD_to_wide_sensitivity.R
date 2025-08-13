# -------------------------------------
# Script: OUD_to_wide
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(tictoc)

source("~/medicaid/post_surgery_opioid_use/R/helpers.R")

load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/intermediate"
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/outcomes"

cohort <- readRDS(file.path(load_dir, "first_surgeries.rds")) |>
  select(BENE_ID, PRCDR_CD, followup_start_dt = discharge_dt)

# months_to_dropout <- readRDS(file.path(save_dir, "cohort_months_to_dropout.rds"))
censoring <- load_data(paste0("pain_washout_continuous_enrollment_censoring.fst"), file.path(drv_root, "sensitivity_analysis/outcomes"))
hillary <- readRDS(file.path(save_dir, "cohort_has_new_hillary.rds"))
oud <- readRDS(file.path(save_dir, "cohort_has_new_oud.rds"))
moud <- readRDS(file.path(save_dir, "cohort_has_new_moud.rds"))


cohort <- cohort |>
  select(BENE_ID, PRCDR_CD, followup_start_dt) |>
  left_join(censoring) |>
  # left_join(poison |> select(BENE_ID, oud_poison_dt)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt)) |>
  left_join(oud |> select(BENE_ID, oud_dt)) |>
  left_join(moud |> select(BENE_ID, moud_start_dt)) |>
  mutate(cens_hillary_period_1 = cens_period_1,
         cens_hillary_period_2 = cens_period_2,
         cens_hillary_period_3 = cens_period_3,
         cens_hillary_period_4 = cens_period_4,
         cens_oud_period_1 = cens_period_1,
         cens_oud_period_2 = cens_period_2,
         cens_oud_period_3 = cens_period_3,
         cens_oud_period_4 = cens_period_4,
         cens_moud_period_1 = cens_period_1,
         cens_moud_period_2 = cens_period_2,
         cens_moud_period_3 = cens_period_3,
         cens_moud_period_4 = cens_period_4,
  )

# cohort2 <- cohort[1:1000,]

for (month in 1:4){
  print(paste("Processing month:", month))
  tic()
  hillary_column <- paste0("oud_hillary_period_", month)
  oud_column <- paste0("oud_period_", month)
  moud_column <- paste0("moud_period_", month)
  
  cohort <- cohort |>
    mutate(
      # {{censor_column}} := ifelse(enrolled_until <= followup_start_dt %m+% months(month*6), 0, 1),
      # {{poison_column}} := ifelse(!is.na(oud_poison_dt) & oud_poison_dt <= followup_start_dt %m+% months(month*6), 1, 0),
      {{hillary_column}} := ifelse(!is.na(oud_hillary_dt) & oud_hillary_dt <= followup_start_dt %m+% months(month*6), 1, 0),
      {{oud_column}} := ifelse(!is.na(oud_dt) & oud_dt <= followup_start_dt %m+% months(month*6), 1, 0),
      {{moud_column}} := ifelse(!is.na(moud_start_dt) & moud_start_dt <= followup_start_dt %m+% months(month*6), 1, 0),
    )
  toc()
}


# saveRDS(cohort, file.path(save_dir, "outcomes_wide_6mos.rds"))
# 
# 
# ###### UNCOMMENT BOTTOM PORTION OF NEEDED
# cohort <- readRDS(file.path(save_dir, "outcomes_wide_6mos.rds"))
# 
# 
# # Carrying forward last value if someone has been censored.
# # Previously, there was a problem where some small number of people would be re-enrolled, and therefore, their outcome variables might change, even though they had been censored earlier.
# # This should not be allowed, so I changed it, for those who were censored, to overwrite all later values with their last uncensored value.
# 
# 
# censored_1 <- which(cohort$C_1 == 0)
# cohort[censored_1, c("Y2_1","Y2_2","Y2_3","Y2_4",
#                      "Y3_1","Y3_2","Y3_3","Y3_4",
#                      "Y4_1","Y4_2","Y4_3","Y4_4")] <- 0
# 
# for (i in 2:4){
#   print(paste("Processing month:", i))
#   
#   tic()
#   censor_column <- paste0("C_", i)
#   last_uncensored <- c(paste0("Y2_", i-1),
#                        paste0("Y3_", i-1),
#                        paste0("Y4_", i-1))
#   
#   censored <- which(cohort[,censor_column] == 0)
#   
#   for (j in i:4){
#     carry_forward_columns <- c(paste0("Y2_", j),
#                                paste0("Y3_", j),
#                                paste0("Y4_", j))
#     cohort[censored, carry_forward_columns] <- cohort[censored, last_uncensored]
#   }
#   toc()
# }
# 
# saveRDS(cohort, file.path(save_dir, "outcomes_wide_6mos_revised.rds"))
# 

convert_cens_to_na <- function (data, outcomes, cens) {
  DT <- as.data.table(data)
  tau <- length(outcomes)
  for (j in 1:(tau)) {
    modify <- setdiff(cens[match(cens[j], cens):tau], cens[j])
    outcome_j <- outcomes[j]
    DT[get(outcome_j) == 1, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
  }
  DT[]
  DT
}

convert_outcome_to_na <- function (data, outcomes, cens) {
  DT <- as.data.table(data)
  tau <- length(outcomes)
  for (j in 1:(tau - 1)) {
    modify <- outcomes[match(outcomes[j], outcomes):tau]
    cens_j <- cens[j]
    DT[get(cens_j) == 0, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
    
    if(j > 1){ # if previously experienced outcome but then censored at later point, considered to have had outcome at subsequent timepoints
      outcome_j_1 <- outcomes[j-1]
      DT[get(outcome_j_1) == 1, `:=`((modify), lapply(.SD, function(x) 1)), .SDcols = modify]
    }
    
    
  }
  DT[]
  DT
}

cohort <- cohort |>
  convert_outcome_to_na(paste0("oud_hillary_period_", 1:4), paste0("cens_hillary_period_", 1:4)) |>
  convert_cens_to_na(paste0("oud_hillary_period_", 1:4), paste0("cens_hillary_period_", 1:4)) |>
  convert_outcome_to_na(paste0("oud_period_", 1:4), paste0("cens_oud_period_", 1:4)) |>
  convert_cens_to_na(paste0("oud_period_", 1:4), paste0("cens_oud_period_", 1:4)) |>
  convert_outcome_to_na(paste0("moud_period_", 1:4), paste0("cens_moud_period_", 1:4)) |>
  convert_cens_to_na(paste0("moud_period_", 1:4), paste0("cens_moud_period_", 1:4)) |>
  mutate(oud_period_4 = case_when(oud_period_3 == 1 ~ 1,
                                  cens_period_4 == 0 ~ as.numeric(NA),
                                  TRUE ~ oud_period_4),
         oud_hillary_period_4 = case_when(oud_hillary_period_3 == 1 ~ 1,
                                          cens_hillary_period_4 == 0 ~ as.numeric(NA),
                                          TRUE ~ oud_hillary_period_4),
         moud_period_4 = case_when(moud_period_3 == 1 ~ 1,
                                   cens_moud_period_4 == 0 ~ as.numeric(NA),
                                   TRUE ~ moud_period_4)
  )

saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/outcomes/outcomes_wide_6mos_revised.rds")




