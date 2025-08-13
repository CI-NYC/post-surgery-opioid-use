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

# Censor first
censor_first_cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/censor_prioritized.rds")

# censor_first_cohort <- censor_first_cohort %>%
#   mutate(time_to_outcome = case_when(
#     Y2_1 == 1 ~ 1,
#     Y2_2 == 1 ~ 2,
#     Y2_3 == 1 ~ 3,
#     Y2_4 == 1 ~ 4,
#     TRUE ~ NA
#   ))
# 
# censor_first_cohort <- censor_first_cohort %>%
#   mutate(time_to_censor = case_when(
#     C_1 == 0 ~ 1,
#     C_2 == 0 ~ 2,
#     C_3 == 0 ~ 3,
#     C_4 == 0 ~ 4,
#     TRUE ~ 5
#   ))
# 
# # Determine the event status
# censor_first_cohort$status <- ifelse(
#   !is.na(censor_first_cohort$time_to_outcome) & 
#     (is.na(censor_first_cohort$time_to_censor) | censor_first_cohort$time_to_outcome <= censor_first_cohort$time_to_censor), 
#   1, 
#   0
# )
# 
# 
# censor_first_cohort$time <- pmin(censor_first_cohort$time_to_outcome, censor_first_cohort$time_to_censor, na.rm = TRUE)

surv_obj <- Surv(censor_first_cohort$time, censor_first_cohort$status)

censor_first_fit <- survfit(surv_obj ~ 1)



# Outcome first
outcome_first_cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/outcome_prioritized.rds")

# outcome_first_cohort <- outcome_first_cohort %>%
#   mutate(time_to_outcome = case_when(
#     Y2_1 == 1 ~ 1,
#     Y2_2 == 1 ~ 2,
#     Y2_3 == 1 ~ 3,
#     Y2_4 == 1 ~ 4,
#     TRUE ~ NA
#   ))
# 
# outcome_first_cohort <- outcome_first_cohort %>%
#   mutate(time_to_censor = case_when(
#     C_1 == 0 ~ 1,
#     C_2 == 0 ~ 2,
#     C_3 == 0 ~ 3,
#     C_4 == 0 ~ 4,
#     TRUE ~ 5
#   ))
# 
# # Determine the event status
# outcome_first_cohort$status <- ifelse(
#   !is.na(outcome_first_cohort$time_to_outcome) & 
#     (is.na(outcome_first_cohort$time_to_censor) | outcome_first_cohort$time_to_outcome <= outcome_first_cohort$time_to_censor), 
#   1, 
#   0
# )
# outcome_first_cohort$time <- pmin(outcome_first_cohort$time_to_outcome, outcome_first_cohort$time_to_censor, na.rm=T)

surv_obj <- Surv(outcome_first_cohort$time, outcome_first_cohort$status)

outcome_first_fit <- survfit(surv_obj ~ 1)


# continuous
continuous_cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
months_to_dropout <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")
hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_has_new_hillary.rds")

continuous_cohort <- continuous_cohort |>
  select(BENE_ID, followup_start_dt) |>
  left_join(months_to_dropout |> select(BENE_ID, enrolled_until)) |>
  left_join(hillary |> select(BENE_ID, oud_hillary_dt))

continuous_cohort$time <- with(continuous_cohort, pmin(enrolled_until, oud_hillary_dt, na.rm = TRUE) - followup_start_dt)
continuous_cohort$status <- with(continuous_cohort, ifelse(!is.na(oud_hillary_dt) & oud_hillary_dt <= enrolled_until, 1, 0))

surv_obj <- with(continuous_cohort, Surv(time, status))

continuous_fit <- survfit(surv_obj ~ 1)



# plots
censor_first_data <- data.frame(
  time = c(0,censor_first_fit$time[1:4]*180),
  surv = c(0,1-censor_first_fit$surv[1:4])
)

outcome_first_data <- data.frame(
  time = c(0,outcome_first_fit$time[1:4]*180),
  surv = c(0,1-outcome_first_fit$surv[1:4])
)

continuous_data <- data.frame(
  time = continuous_fit$time,
  surv = 1-continuous_fit$surv
  # ,upper = continuous_fit$upper,
  # lower = continuous_fit$lower
)
# ggplot(censor_first_data, aes(x = time, y = surv)) +
#   geom_step() +
#   labs(x = "Time", y = "Survival Probability", title = "Survival Curve") +
#   theme_minimal()
# 
# ggplot(outcome_first_data, aes(x = time, y = surv)) +
#   geom_step() +
#   labs(x = "Time", y = "Survival Probability", title = "Survival Curve") +
#   theme_minimal()
# 
# ggplot(continuous_data, aes(x = time, y = surv)) +
#   geom_step() +
#   labs(x = "Time", y = "Survival Probability", title = "Survival Curve") +
#   xlim(0,730) +
#   theme_minimal()

# library(ggplot2)

# Combine the data frames into one
combined_data <- rbind(
  data.frame(time = censor_first_data$time, surv = censor_first_data$surv, group = "Discretized - ignore outcome if censored"),
  data.frame(time = outcome_first_data$time, surv = outcome_first_data$surv, group = "Discretized - prioritize outcome"),
  data.frame(time = continuous_data$time, surv = continuous_data$surv, group = "Continuous")
)

# Plot the combined data
ggplot(combined_data, aes(x = time, y = surv, color = group)) +
  geom_step() +
  labs(x = "Time", y = "Cumulative Incidence", title = "") +
  xlim(0, 730) +
  theme_minimal()





