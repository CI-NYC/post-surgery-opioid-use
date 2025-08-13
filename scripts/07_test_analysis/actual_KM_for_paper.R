# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(ggplot2)
library(survival)
library(dplyr)
library(data.table)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
months_to_dropout <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")
hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")

cohort <- cohort |>
  select(BENE_ID, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt))

continuous_cohort$time <- with(cohort, pmin(enrolled_until, oud_hillary_dt, na.rm = TRUE) - followup_start_dt)
continuous_cohort$status <- with(cohort, ifelse(!is.na(oud_hillary_dt) & oud_hillary_dt <= enrolled_until, 1, 0))

surv_obj <- with(continuous_cohort, Surv(time, status))

continuous_fit <- survfit(surv_obj ~ 1)

continuous_data <- data.frame(
  time = continuous_fit$time,
  surv = 1-continuous_fit$surv
)

# Discretized - ignore outcome if censored

censor_first_cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/censor_prioritized.rds")

surv_obj <- Surv(censor_first_cohort$time, censor_first_cohort$status)

censor_first_fit <- survfit(surv_obj ~ 1)


# current method used in the paper - has a mistake
setDT(cohort)

cohort$time_to_outcome <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$oud_hillary_dt) / months(1) 
  / 6)
cohort$time_to_censor <- ceiling(
  interval(cohort$followup_start_dt-1,cohort$enrolled_until) / months(1) 
  / 6) - 1

cohort <- cohort[, status := !is.na(time_to_outcome) & 
                   time_to_outcome <= time_to_censor]

cohort$status = as.numeric(cohort$status)

cohort$time <- pmin(cohort$time_to_censor,cohort$time_to_outcome, na.rm=T)

cohort <- cohort |>
  select(BENE_ID, status,time)

surv_obj <- Surv(cohort$time, cohort$status)

outcome_first_fit <- survfit(surv_obj ~ 1)


censor_first_data <- data.frame(
  time = c(0,censor_first_fit$time[1:5]*180),
  surv = c(0,1-censor_first_fit$surv[1:5])
)

outcome_first_data <- data.frame(
  time = c(0,outcome_first_fit$time[1:6]*180),
  surv = c(0,1-outcome_first_fit$surv[1:6])
)


combined_data <- rbind(
  data.frame(time = censor_first_data$time, surv = censor_first_data$surv, group = "Discretized - ignore outcome if censored"),
  data.frame(time = outcome_first_data$time, surv = outcome_first_data$surv, group = "Current - has a mistake"),
  data.frame(time = continuous_data$time, surv = continuous_data$surv, group = "Continuous")
)

# Plot the combined data
ggplot(combined_data, aes(x = time, y = surv, color = group)) +
  geom_step() +
  labs(x = "Time", y = "Cumulative Incidence", title = "") +
  xlim(0, 910) +
  theme_minimal()
