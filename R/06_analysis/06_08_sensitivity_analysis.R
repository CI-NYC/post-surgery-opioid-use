# -------------------------------------
# Script: survival
# Author: Anton Hung
# Date: June 22
# Purpose:
# Notes:
# -------------------------------------

library(lmtp)
library(tictoc)
library(tidyverse)
library(lubridate)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners) 
library(purrr)

dat_non_c_section <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/final/df_non_c_section.rds") |>
  select(-days_of_continuous_use) |>
  as.data.frame()

dat_only_c_section <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/final/df_only_c_section.rds") |>
  select(-days_of_continuous_use) |>
  as.data.frame()

# dat_non_c_section <- dat_non_c_section[1:10000,]

# combining the three individual MOUD categories together
# dat_lmtp <- dat_lmtp |>
#   mutate(Y6_1 = ifelse(Y3_1 == 1 | Y4_1 == 1 | Y5_1 == 1, 1, 0),
#          Y6_2 = ifelse(Y3_2 == 1 | Y4_2 == 1 | Y5_2 == 1, 1, 0),
#          Y6_3 = ifelse(Y3_3 == 1 | Y4_3 == 1 | Y5_3 == 1, 1, 0),
#          Y6_4 = ifelse(Y3_4 == 1 | Y4_4 == 1 | Y5_4 == 1, 1, 0))

shift_data <- function(dat_lmtp, shift_columns){
  shited_data <- dat_lmtp |>
    mutate_at(shift_columns, ~ . * 0.8) |>
    mutate(C_1 = 1,
           C_2 = 1,
           C_3 = 1,
           C_4 = 1)
}


# # create 5 shifted data sets
# shift_1 <- dat_non_c_section |> 
#   mutate(mean_daily_dose_mme = 0.8*mean_daily_dose_mme,
#          days_supplied = 0.8*days_supplied,
#          days_of_continuous_use = 0.8*days_of_continuous_use,
#          C_1 = 1,
#          C_2 = 1,
#          C_3 = 1,
#          C_4 = 1)
# 
# shift_2 <- dat_lmtp |>
#   mutate(mean_daily_dose_mme = 0.8*mean_daily_dose_mme,
#          days_supplied =days_supplied,
#          days_of_continuous_use = days_of_continuous_use,
#          C_1 = 1,
#          C_2 = 1,
#          C_3 = 1,
#          C_4 = 1)
# 
# shift_3 <- dat_lmtp |>
#   mutate(mean_daily_dose_mme = mean_daily_dose_mme,
#          days_supplied = 0.8*days_supplied,
#          days_of_continuous_use = days_of_continuous_use,
#          C_1 = 1,
#          C_2 = 1,
#          C_3 = 1,
#          C_4 = 1)
# 
# shift_4 <- dat_lmtp |>
#   mutate(mean_daily_dose_mme = mean_daily_dose_mme,
#          days_supplied = days_supplied,
#          days_of_continuous_use = 0.8*days_of_continuous_use,
#          C_1 = 1,
#          C_2 = 1,
#          C_3 = 1,
#          C_4 = 1)
# 
# shift_5 <- dat_lmtp |>
#   mutate(mean_daily_dose_mme = mean_daily_dose_mme,
#          days_supplied = 0.8*days_supplied,
#          days_of_continuous_use = 0.8*days_of_continuous_use,
#          C_1 = 1,
#          C_2 = 1,
#          C_3 = 1,
#          C_4 = 1)

# identifying censor columns
C <- dat_non_c_section |>
  select(starts_with("C")) |>
  names()

# identifying exposure columns
A <- list(c("mean_daily_dose_mme", "days_supplied")) # removed days_supplied

# learners
libs <- c("mean", "glm", "xgboost", "earth")

survival_lmtp_intervention <- function(Y, W, data, shifted, shift_name){
  tau = 4
  # results_interv <- vector("list", length = tau)
  
  print(paste0("Processing outcome: ", Y, ", version: ", shift_name))
  results_interv <- 
    progressr::with_progress({
      lmtp_survival(
        data,
        trt = A,
        outcome = paste0(Y, "_", 1:tau),
        baseline = W,
        cens = paste0("C_", 1:tau),
        mtp = T,
        learners_outcome = libs,
        learners_trt = libs,
        shifted = shifted,
        estimator = "lmtp_tmle",
        folds = 5, 
        control = lmtp_control(.learners_outcome_folds = 2,
                               .learners_trt_folds = 2)
      )
    })
  # toc()
  print(date())
  
  # for (t in 1:4) {
  #   # tic()
  #   print(paste0("Processing outcome: ", Y, ", version: ", shift_name, ", t: ", t))
  #   results_interv[[t]] <- 
  #     progressr::with_progress({
  #       lmtp_tmle(
  #         data,
  #         trt = A,
  #         outcome = paste0(Y, "_", 1:t),
  #         baseline = W,
  #         cens = paste0("C_", 1:t),
  #         mtp = T,
  #         learners_outcome = libs,
  #         learners_trt = libs,
  #         outcome_type = ifelse(t == 1, "binomial", "survival"),
  #         shifted = shifted,
  #         folds = 5, 
  #         control = lmtp_control(.learners_outcome_folds = 2,
  #                                .learners_trt_folds = 2)
  #       )
  #     })
  #   # toc()
  #   print(date())
  # }
  return(results_interv)
}




survival_lmtp_observed <- function(Y, W, data){
  tau = 4
  # results_observ <- vector("list", length = tau)
  
  print(paste0("Processing outcome: ", Y, ", version: observed"))
  results_observ <- 
    progressr::with_progress({
      lmtp_survival(
        data,
        trt = A,
        outcome = paste0(Y, "_", 1:tau),
        baseline = W,
        cens = paste0("C_", 1:tau),
        mtp = T,
        learners_outcome = libs,
        learners_trt = libs,
        # shifted = shifted,
        estimator = "lmtp_tmle",
        folds = 5, 
        control = lmtp_control(.learners_outcome_folds = 2,
                               .learners_trt_folds = 2)
      )
    })
  
  # toc()
  print(date())
  
  # for (t in 1:tau) {
  #   # tic()
  #   print(paste0("Processing outcome: ", Y, ", version: observed, t: ", t))
  #   results_observ[[t]] <- 
  #     progressr::with_progress({
  #       lmtp_tmle(
  #         data,
  #         trt = A,
  #         outcome = paste0(Y, "_", 1:t),
  #         baseline = W,
  #         cens = paste0("C_", 1:t),
  #         learners_outcome = libs,
  #         learners_trt = libs,
  #         outcome_type = ifelse(t == 1, "binomial", "survival"),
  #         folds = 5, 
  #         control = lmtp_control(.learners_outcome_folds = 2,
  #                                .learners_trt_folds = 2)
  #       )
  #     })
  #   # toc()
  #   print(date())
  # }
  return(results_observ)
}




lmtp_contrast_and_save <- function(Y, W, data, shifted, shift_name, results_observ, c_section_identifier){
  tau = 4
  results_contrast <- vector("list", length = tau)
  
  results_interv <- survival_lmtp_intervention(Y, W, data, shifted, shift_name)
  
  # compare results
  for (t in 1:tau) { 
    results_contrast[[t]] <- lmtp_contrast(results_interv[[t]], ref = results_observ[[t]]) 
  }
  
  results <- list(results_interv,
                  results_observ,
                  results_contrast)
  names(results) <- c("intervention", "observed", "contrast")
  saveRDS(results, file.path("/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/analysis",
                             paste0("lmtp_result_", c_section_identifier, "_", shift_name, "_", Y, ".rds")))
}


set.seed(1)
# for (Y in c("Y1","Y2","Y6")){
#   my_survival_lmtp(Y, dat_lmtp, shift_1, "shift_1")
#   my_survival_lmtp(Y, dat_lmtp, shift_2, "shift_2")
#   my_survival_lmtp(Y, dat_lmtp, shift_3, "shift_3")
#   my_survival_lmtp(Y, dat_lmtp, shift_4, "shift_4")
#   my_survival_lmtp(Y, dat_lmtp, shift_5, "shift_5")
#   
# }


W_non_c_section <- dat_non_c_section |> # all confounder vars are together from data cleaning
  select(age_enrollment:length_of_stay) |>
  names()

W_only_c_section <- dat_only_c_section |>
  select(age_enrollment:has_pain_back_neck_unspecified) |>
  names()

run_lmtp <- function(dat_lmtp, W, c_section_identifier) {
  shift_1 <- shift_data(dat_lmtp, c("mean_daily_dose_mme", "days_supplied"))
  shift_2 <- shift_data(dat_lmtp, c("mean_daily_dose_mme"))
  # shift_3 <- shift_data(dat_lmtp, c("days_supplied"))
  shift_3 <- shift_data(dat_lmtp, c("days_supplied"))
  # shift_5 <- shift_data(dat_lmtp, c("days_supplied", "days_of_continuous_use"))
  
  for (Y in c("Y2", "Y3","Y4")){
    
    results_observ <- survival_lmtp_observed(Y, W, dat_lmtp)
    
    lmtp_contrast_and_save(Y, W, dat_lmtp, shift_1, "shift_1", results_observ, c_section_identifier)
    lmtp_contrast_and_save(Y, W, dat_lmtp, shift_2, "shift_2", results_observ, c_section_identifier)
    lmtp_contrast_and_save(Y, W, dat_lmtp, shift_3, "shift_3", results_observ, c_section_identifier)
    # lmtp_contrast_and_save(Y, W, dat_lmtp, shift_4, "shift_4", results_observ, c_section_identifier)
    # lmtp_contrast_and_save(Y, W, dat_lmtp, shift_5, "shift_5", results_observ, c_section_identifier)
  } 
}


run_lmtp(dat_non_c_section, W_non_c_section, "other")
run_lmtp(dat_only_c_section, W_only_c_section, "c-section")

