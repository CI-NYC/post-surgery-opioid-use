# -------------------------------------
# Script: survival
# Author:
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

dat_lmtp <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/combined_df.rds") |>
  select(-has_pain_back_neck_unspecified) |>
  as.data.frame()
# dat_lmtp <- readRDS("/mnt/general-data/disability/create_cohort/final/dat_lmtp.rds")

# for (i in 1:24){
#   
#   which_6mos <- ifelse(floor(i/6) == 0, 1,
#                       ifelse(floor(i/6) == 1, 2,
#                              ifelse(floor(i/6) == 2, 3, 4)))
#   
#   censor_column <- paste0("C_", which_6mos)
#   OD_column <- paste0("Y1_", which_6mos)
#   OUD_column <- paste0("Y2_", which_6mos)
#   met_column <- paste0("Y3_", which_6mos)
#   nal_column <- paste0("Y4_", which_6mos)
#   bup_column <- paste0("Y5_", which_6mos)
#   
#   C <- paste0("")
#   # columns
#   columns <- C, Y1, Y2, Y3, Y4, Y5
#   paste0(columns, i)
#   
#   
#   dat_lmtp <- dat_lmtp |>
#     mutate({{censor_column}} = ,
#            {{OD_column}} = ,
#            {{OUD_column}} = ,
#            {{met_column}} = ,
#            {{nal_column}} = ,
#            {{bup_column}})
# }

# create shifted data set
shift_1 <- dat_lmtp |> 
  mutate(mean_daily_dose_mme = 0.8*mean_daily_dose_mme,
         days_supplied = 0.8*days_supplied,
         days_of_continuous_use = 0.8*days_of_continuous_use,
         C_1 = 1,
         C_2 = 1,
         C_3 = 1,
         C_4 = 1)

Y <- dat_lmtp |>
  select(starts_with("Y1")) |>
  names()
C <- dat_lmtp |>
  select(starts_with("C")) |>
  names()
A <- list(c("mean_daily_dose_mme", "days_supplied", "days_of_continuous_use"))
# A <- "mean_daily_dose_mme"

W <- dat_lmtp |> # all confounder vars are together from data cleaning
  select(SEX_M:surgery_major) |>
  names()

libs <- c("mean", "glm", "xgboost", "earth")

# dat_lmtp <- dat_lmtp[1:10000,]
# shift_1 <- shift_1[1:10000,]


for (Y in c("Y1","Y2","Y3","Y4","Y5")){
  set.seed(1)
  # Y <- c(paste0(Y,"_1"),
  #        paste0(Y,"_2"),
  #        paste0(Y,"_3"),
  #        paste0(Y,"_4"))
  
  # dat_lmtp <- event_locf(dat_lmtp, Y)
  
  tic()
  # fit1 <-
  #   progressr::with_progress({
  #     lmtp_tmle(dat_lmtp_sample,
  #               trt = A,
  #               outcome = Y,
  #               baseline = W,
  #               cens = C,
  #               mtp = T,
  #               folds = 1,
  #               learners_outcome = libs,
  #               learners_trt = libs,
  #               outcome_type = "survival",
  #               shifted = shift_1_sample,
  #               # control = lmtp_control(.learners_trt_folds = 2, .learners_outcome_folds = 2)
  #     )
  #   }
  #   )
  
  tau <- 4
  results_interv <- vector("list", length = tau)
  results_observ <- vector("list", length = tau)
  results_contrast <- vector("list", length = tau)
  
  # intervention results
  for (t in 1:tau) {
    results_interv[[t]] <- 
      progressr::with_progress({
        lmtp_tmle(
        dat_lmtp,
        trt = A,
        outcome = paste0(Y, "_", 1:t),
        baseline = W,
        cens = paste0("C_", 1:t),
        mtp = T,
        learners_outcome = libs,
        learners_trt = libs,
        outcome_type = ifelse(t == 1, "binomial", "survival"),
        shifted = shift_1,
        folds = 1, 
        control = lmtp_control(.learners_outcome_folds = 2, 
                               .learners_trt_folds = 2)
        )
      })
  }
  
  # observed results
  for (t in 1:tau) {
    results_observ[[t]] <- 
      progressr::with_progress({
        lmtp_tmle(
          dat_lmtp,
          trt = A,
          outcome = paste0(Y, "_", 1:t),
          baseline = W,
          cens = paste0("C_", 1:t),
          learners_outcome = libs,
          learners_trt = libs,
          outcome_type = ifelse(t == 1, "binomial", "survival"),
          folds = 1, 
          control = lmtp_control(.learners_outcome_folds = 2, 
                                 .learners_trt_folds = 2)
        )
      })
  }
  
  
  # compare results
  for (t in 1:tau) {
    results_contrast[[t]] <- lmtp_contrast(results_interv[[t]], ref = results_observ[[t]])
  }
  
  toc()

  results <- list(results_interv,
                  results_observ,
                  results_contrast)
  names(results) <- c("intervention", "observed", "contrast")
  saveRDS(results, file.path("/mnt/general-data/disability/post_surgery_opioid_use/analysis", 
                             paste0("lmtp_result_", Y, ".rds")))
}



### Survival curve
results_tidy <- map_dfr(results_observ, tidy, .id = "t") |> 
  mutate(estimate = ifelse(t == 1, 1 - estimate, estimate), 
         t = as.numeric(t))

print(results_tidy)

ggplot(results_tidy, aes(x = t, y = estimate)) + 
  geom_step() + 
  labs(x = "6-month period", y = "Survival probability") + 
  # scale_y_continuous(limits = c(0, 1), 
  #                    n.breaks = 10, 
  #                    expand = c(0, 0)) + 
  scale_x_continuous(limits = c(0, 4), 
                     n.breaks = 4, 
                     expand = c(0.01, 0))


#### Single time point check

# result_1 <-
#   progressr::with_progress({
#     lmtp_tmle(
#       dat_lmtp,
#       trt = A,
#       outcome = paste0("Y2_", 1:4),
#       baseline = W,
#       cens = paste0("C_", 1:4),
#       mtp = T,
#       learners_outcome = libs,
#       learners_trt = libs,
#       outcome_type = "survival",
#       shifted = shift_1,
#       folds = 1,
#       control = lmtp_control(.learners_outcome_folds = 2,
#                              .learners_trt_folds = 2)
#     )
#   })

# for (j in 6:19) {
#   print(table(dat_lmtp[which(dat_lmtp[,j] == 1),]$Y4_1))
# }
