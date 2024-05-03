# -------------------------------------
# Script: months_to_dropout
# Author: Anton Hung
# Purpose: 
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(dplyr)
library(survival)
library(ggplot2)

# read surgery claims
cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
setDT(cohort)
# claims <- claims[1:1000]

dts_cohort <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdedts/nest_dts.rds")
setDT(dts_cohort)

cohort <- dts_cohort[BENE_ID %in% cohort$BENE_ID] |>
  left_join(cohort[, c("BENE_ID", "discharge_dt")])


cohort$enrolled_until <- as.Date(sapply(1:nrow(cohort), function(i) {
  start_dt <- cohort[i, discharge_dt]
  
  enrollments <- cohort[[i, "data"]] |>
    group_by(ENRLMT_START_DT) |>
    mutate(ENRLMT_END_DT = max(ENRLMT_END_DT)) |>
    slice(1) |>
    ungroup() |> 
    mutate(ascending = ENRLMT_END_DT >= lag(ENRLMT_END_DT, default = first(ENRLMT_END_DT))) |>
    filter(ascending == T)
  
  enrollment_date <- enrollments$ENRLMT_START_DT
  enrollment_end <- enrollments$ENRLMT_END_DT
  last_enrollment_end <- lag(enrollment_end)
  
  # QUESTION 1: is the start date contained within this row?
  q1 <- start_dt %within% interval(enrollment_date, enrollment_end)
  # QUESTION 2: which periods are continuous with the previous periods
  # finding the first success in q1, and getting all later successes in q2
  # finding the last row that is continuous with the previous row
  
  # q2 <- (last_enrollment_end + days(1)) == enrollment_date
  q2 <- (last_enrollment_end + days(60)) >= enrollment_date
  
  q2[1:which(q1)] = T # setting all previous indices to T as a placeholder
  last_true_index <- ifelse(all(q2), length(q2), which.min(q2)-1) # finding the final consecutive continuous enrollment period after start_dt  
  
  # return the end date where the last T in q2 is found
  end_dt <- enrollment_end[last_true_index]
  
  
  # return the length of time separating the end date from the start date
  # return (time_length(end_dt - start_dt, "months"))
  return(end_dt)
}))

cohort <- cohort |>
  mutate(months_to_dropout = time_length(enrolled_until - discharge_dt, "months"))

cohort <- cohort |>
  select(BENE_ID, discharge_dt, enrolled_until, months_to_dropout)
saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")


# cohort <- cohort |>
#   # mutate(check = any(discharge_dt %within% interval(data[[1]][["ENRLMT_START_DT"]], data[[1]][["ENRLMT_END_DT"]])))
#   mutate(check = any(discharge_dt %within% interval(data[[1]][["ENRLMT_START_DT"]], data[[1]][["ENRLMT_END_DT"]])))


########### some plots ###########

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/outcomes/cohort_months_to_dropout.rds")

cohort <- cohort |>
  mutate(months_to_dropout = round(months_to_dropout)) |>
  mutate(months_to_dropout = ifelse(months_to_dropout > 24, 24, months_to_dropout)) |>
  mutate(status = months_to_dropout >= 24)

# ggplot(cohort, aes(x=months_to_dropout)) + geom_histogram()

surv_obj <- with(cohort, Surv(months_to_dropout, status == 0))

# Fit a survival curve
fit <- survfit(surv_obj ~ 1)

pdf("/home/amh2389/medicaid/post_surgery_opioid_use/output/months_to_dropout.pdf")

plot(fit, 
     xlab = "Months elapsed", 
     ylab = "Proportion remaining", 
     main = "Proportion of beneficiaries still enrolled in Medicaid by month",
     xaxt = "n",
     col = "skyblue",
     lwd = 2)

ticks <- c(0, 3, 6, 9, 12, 15, 18, 21, 24)
axis(side = 1, at = ticks, labels = ticks)

# horizontal and vertical gridlines
abline(v = ticks, col = "lightgray", lty = "dotted")
abline(h = pretty(range(0,1)), col = "lightgray", lty = "dotted")

dev.off()