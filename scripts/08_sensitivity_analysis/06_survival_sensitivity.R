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

# paramaters to modify
# learners
libs <- c("mean", "glm", "xgboost", "earth")
CF_folds <- 4
SL_folds <- 2
version <- "xgboost"
Ys <- c("Y2","Y3","Y4")
print(paste0("CF_folds: ", CF_folds, ", Version: ", version, ", ", paste(Ys)))

load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/final"
save_dir <- file.path("/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/analysis", version)

dat_non_c_section <- readRDS(file.path(load_dir, "df_non_c_section.rds")) |>
  select(-days_of_continuous_use) |>
  as.data.frame()
# dat_non_c_section <- dat_non_c_section[1:1000,]

dat_only_c_section <- readRDS(file.path(load_dir, "df_only_c_section.rds")) |>
  select(-days_of_continuous_use) |>
  as.data.frame()
# dat_only_c_section <- dat_only_c_section[1:1000,]


shift_data <- function(dat_lmtp, shift_columns){
  shited_data <- dat_lmtp |>
    # mutate_at(vars(shift_columns), ~ pmax(. * 0.8, min(.))) |>
    mutate_at(shift_columns, ~ . * 0.8) |>
    mutate(C_1 = 1,
           C_2 = 1,
           C_3 = 1,
           C_4 = 1)
}



# identifying censor columns
C <- dat_non_c_section |>
  select(starts_with("C")) |>
  names()

# identifying exposure columns
A <- list(c("mean_daily_dose_mme", "days_supplied")) # removed days_supplied



survival_lmtp_intervention <- function(Y, W, data, shifted, shift_name){
  tau = 4
  # results_interv <- vector("list", length = tau)
  
  # print(paste0("Processing outcome: ", Y, ", version: ", shift_name))
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
        folds = CF_folds, 
        control = lmtp_control(.learners_outcome_folds = SL_folds,
                               .learners_trt_folds = SL_folds)
      )
    })
  # toc()
  print(date())
  
  return(results_interv)
}




survival_lmtp_observed <- function(Y, W, data){
  tau = 4
  # results_observ <- vector("list", length = tau)
  
  # print(paste0("Processing outcome: ", Y, ", version: observed"))
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
        folds = CF_folds, 
        control = lmtp_control(.learners_outcome_folds = SL_folds,
                               .learners_trt_folds = SL_folds)
      )
    })
  
  # toc()
  print(date())
  
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
  saveRDS(results, file.path(save_dir, paste0("lmtp_result_", c_section_identifier, "_", shift_name, "_", Y, ".rds")))
  
}


set.seed(11)


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
  
  for (Y in Ys){
    tic(paste0("Processing outcome: ", Y, ", observed"))
    results_observ <- survival_lmtp_observed(Y, W, dat_lmtp)
    toc()
    # 
    # saveRDS(results_observ, file.path(save_dir, paste0("observed_", c_section_identifier, "_", Y, ".rds")))
    # results_observ <- readRDS(file.path(save_dir, paste0("observed_", c_section_identifier, "_", Y, ".rds")))
    tic(paste0("Processing outcome: ", Y, ", shift_1"))
    lmtp_contrast_and_save(Y, W, dat_lmtp, shift_1, "shift_1", results_observ, c_section_identifier)
    toc()

    tic(paste0("Processing outcome: ", Y, ", shift_2"))
    lmtp_contrast_and_save(Y, W, dat_lmtp, shift_2, "shift_2", results_observ, c_section_identifier)
    toc()

    tic(paste0("Processing outcome: ", Y, ", shift_3"))
    lmtp_contrast_and_save(Y, W, dat_lmtp, shift_3, "shift_3", results_observ, c_section_identifier)
    toc()
  } 
}


# run_lmtp(dat_non_c_section, W_non_c_section, "other")
run_lmtp(dat_only_c_section, W_only_c_section, "c-section")

