# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(ggplot2)

days_supplied <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/surgery_days_supplied.rds")

days_continuous <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/surgeries_opioids_days_continuous.rds")

daily_mme <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/opioid_data/surgery_mean_daily_dose_mme.rds")


ggplot(daily_mme) +
  geom_histogram(aes(x = mediator_mean_daily_dose_mme)) +
  xlab("daily MME")

sum(daily_mme$mediator_mean_daily_dose_mme > 150)

ggplot(days_supplied) +
  geom_histogram(aes(x = days_supplied)) +
  xlab("days supplied")


days_continuous$max_days <- sapply(days_continuous$days_of_continuous_use, max)

ggplot(days_continuous) +
  geom_histogram(aes(x = max_days)) +
  xlab("longest period of continuous use")



surgeries <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds") |>
  select(BENE_ID, surgery_dt, discharge_dt) |>
  mutate(perioperative_length = time_length(interval(surgery_dt %m-% months(1),
                                                     discharge_dt %m+% days(14)), "days"))


days_supplied <- left_join(days_supplied, surgeries, by = "BENE_ID")


ggplot(days_supplied) +
  geom_histogram(aes(x = perioperative_length), binwidth = 1) +
  xlab("perioperative length") +
  xlim(39,51)

days_supplied <- days_supplied |>
  mutate(prop_days = days_supplied/perioperative_length)


ggplot(days_supplied) +
  geom_histogram(aes(x = prop_days)) +
  xlab("Proportion of days supplied")
 