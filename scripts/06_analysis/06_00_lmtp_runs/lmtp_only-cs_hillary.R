# -------------------------------------
# Script: survival
# Author: Anton Hung
# Date: Aug 08 2025
# Purpose:
# Notes:
# -------------------------------------

.libPaths(c("~/libs", .libPaths()))

library(lmtp)
# library(devtools)
# load_all("~/libs/temp-lmtp")
library(tictoc)
library(tidyverse)
library(lubridate)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners) 
library(purrr)
library(xgboost)
library(earth)
library(future)

source("~/medicaid/post_surgery_opioid_use/R/helpers.R")

# paramaters to modify
# learners
sl <- c("SL.mean", "SL.glm", "SL.earth")
CF_folds <- 4
SL_folds <- 2
version <- "earth"
Ys <- c("oud_hillary_period")
Cs <- c("cens_hillary_period_")
print(paste0("CF_folds: ", CF_folds, ", Version: ", version, ", ", paste(Ys)))

dat_non_c_section <- load_data("df_non_c_section.fst", file.path(drv_root, "final")) |>
  as.data.frame()

dat_only_c_section <- load_data("df_only_c_section.fst", file.path(drv_root, "final")) |>
  as.data.frame()

# dat_only_c_section <- dat_only_c_section[1:1000,]

# dat_non_c_section <- dat_non_c_section[1:1000,]


shift_data <- function(dat_lmtp, shift_columns){
  shifted_data <- dat_lmtp |>
    # mutate_at(vars(shift_columns), ~ pmax(. * 0.8, min(.))) |>
    mutate_at(shift_columns, ~ . * 0.8) |>
    mutate_at(vars(all_of(paste0(Cs, 1:4))), ~ 1)
}



# identifying exposure columns
A <- list(c("exposure_mean_daily_dose_mme", "exposure_days_supply"))



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
        cens = paste0(Cs, 1:tau),
        mtp = T,
        learners_outcome = sl,
        learners_trt = sl,
        shifted = shifted,
        estimator = "lmtp_tmle",
        folds = CF_folds, 
        control = lmtp_control(.learners_outcome_folds = SL_folds,
                               .learners_trt_folds = SL_folds,
                               .discrete = F)
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
  
  # print(paste0("Processing outcome: ", Y, ", version: observed"))
  results_observ <- 
    progressr::with_progress({
      lmtp_survival(
        data,
        trt = A,
        outcome = paste0(Y, "_", 1:tau),
        baseline = W,
        cens = paste0(Cs, 1:tau),
        mtp = T,
        learners_outcome = sl,
        learners_trt = sl,
        # shifted = shifted,
        estimator = "lmtp_tmle",
        folds = CF_folds, 
        control = lmtp_control(.learners_outcome_folds = SL_folds,
                               .learners_trt_folds = SL_folds,
                               .discrete = F)
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
  saveRDS(results, file.path("/mnt/general-data/disability/post_surgery_opioid_use/analysis", version,
                             paste0("lmtp_result_", c_section_identifier, "_", shift_name, "_", Y, ".rds")))
}


set.seed(11)



W_non_c_section <- dat_non_c_section |> # all confounder vars are together from data cleaning
  select(age_enrollment:length_of_stay) |>
  names()

W_only_c_section <- dat_only_c_section |>
  select(age_enrollment:has_pain_back_neck_unspecified) |>
  names()

run_lmtp <- function(dat_lmtp, W, c_section_identifier) {
  shift_1 <- shift_data(dat_lmtp, c("exposure_mean_daily_dose_mme", "exposure_days_supply"))
  shift_2 <- shift_data(dat_lmtp, c("exposure_mean_daily_dose_mme"))
  shift_3 <- shift_data(dat_lmtp, c("exposure_days_supply"))
  
  for (Y in Ys){
    tic(paste0("Processing outcome: ", Y, ", observed"))
    results_observ <- survival_lmtp_observed(Y, W, dat_lmtp)
    toc()
    
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

plan(multisession, workers = 4)

# run_lmtp(dat_non_c_section, W_non_c_section, "other")
run_lmtp(dat_only_c_section, W_only_c_section, "c-section")

