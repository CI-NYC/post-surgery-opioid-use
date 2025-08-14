# -------------------------------------
# Script: make_table_one
# Author: Anton Hung
# Date: June 24 2024
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
# library(survival)

source("~/medicaid/post_surgery_opioid_use/R/helpers.R")

df1 <- load_data("df_non_c_section.fst", file.path(drv_root, "sensitivity_analysis/final"))

df2 <- load_data("df_only_c_section.fst", file.path(drv_root, "sensitivity_analysis/final"))

df1 <- df1 |>
  mutate(sex = NA, .before = SEX_M) |>
  mutate(SEX_F = 1 - SEX_M, .after=SEX_M) |>
  mutate(psychiatric_conditions = NA, .before = has_anxiety) |>
  mutate(substance_abuse = NA, .before = has_alcohol_abuse) |>
  mutate(pain = NA, .before = has_pain_back) |>
  mutate(surgery_type = NA, .before = surgery_major) |>
  mutate(surgery_minor = 1 - surgery_major, .after=surgery_major) |>
  relocate(multiple_opioids, .before = length_of_stay) |>
  mutate(single_opioid = 1- multiple_opioids, .before = multiple_opioids) |>
  mutate(num_opioids = NA, .before = single_opioid) |>
  as.data.table()

df2 <- df2 |>
  mutate(sex = NA, .before = SEX_M) |>
  mutate(SEX_M = NA) |>
  mutate(SEX_F = NA, .after=SEX_M) |>
  mutate(psychiatric_conditions = NA, .before = has_anxiety) |>
  mutate(substance_abuse = NA, .before = has_alcohol_abuse) |>
  mutate(pain = NA, .before = has_pain_back) |>
  mutate(surgery_type = NA, .before = surgery_major) |>
  mutate(surgery_major = NA) |>
  mutate(surgery_minor = NA, .after=surgery_major) |>
  relocate(multiple_opioids, .before = length_of_stay) |>
  mutate(single_opioid = 1- multiple_opioids, .before = multiple_opioids) |>
  mutate(num_opioids = NA, .before = single_opioid) |>
  as.data.table()


demographics <- c("Age",
                  "Sex:",
                  "\\hspace{0.5cm}Male",
                  "\\hspace{0.5cm}Female",
                  "Psychiatric Conditions:",
                  "\\hspace{0.5cm}Anxiety",
                  "\\hspace{0.5cm}Depression",
                  "\\hspace{0.5cm}Bipolar",
                  "Substance Abuse Disorders:",
                  "\\hspace{0.5cm}Alcohol",
                  "\\hspace{0.5cm}Other substances",
                  "\\hspace{0.5cm}Nicotine",
                  "Pain Disorders:",
                  "\\hspace{0.5cm}Back pain",
                  "\\hspace{0.5cm}Neck pain",
                  "\\hspace{0.5cm}Arthritis",
                  "\\hspace{0.5cm}Neurological pain",
                  "\\hspace{0.5cm}Headache",
                  "\\hspace{0.5cm}Miscellaneous pain",
                  "\\hspace{0.5cm}Unspecified pain, back or neck",
                  "Surgery Type:",
                  "\\hspace{0.5cm}Major",
                  "\\hspace{0.5cm}Minor",
                  "Number of prescriptions:",
                  "\\hspace{0.5cm}1",
                  "\\hspace{0.5cm}>1",
                  "Length of hospital stay (days):",
                  "\\hspace{0.5cm}1 day",
                  "\\hspace{0.5cm}2 days",
                  "\\hspace{0.5cm}3 days",
                  "\\hspace{0.5cm}4 days",
                  "\\hspace{0.5cm}5 days",
                  "\\hspace{0.5cm}6 days",
                  "\\hspace{0.5cm}7 days",
                  "Dose (MME)",
                  "Days supplied",
                  "OUD (ICD only):",
                  "\\hspace{0.5cm}at 6 months",
                  "\\hspace{0.5cm}at 12 months",
                  "\\hspace{0.5cm}at 18 months",
                  "\\hspace{0.5cm}at 24 months",
                  "MOUD:",
                  "\\hspace{0.5cm}at 6 months",
                  "\\hspace{0.5cm}at 12 months",
                  "\\hspace{0.5cm}at 18 months",
                  "\\hspace{0.5cm}at 24 months",
                  "OUD (comprehensive):",
                  "\\hspace{0.5cm}at 6 months",
                  "\\hspace{0.5cm}at 12 months",
                  "\\hspace{0.5cm}at 18 months",
                  "\\hspace{0.5cm}at 24 months")

age <- paste0(median(df1$age_enrollment)," (",
              quantile(df1$age_enrollment, 0.25),", ",
              quantile(df1$age_enrollment, 0.75),")")

number <- sapply(df1[,5:29], function(x) sum(x))
proportion <- sapply(df1[,5:29], function(x) prop.table(table(x))[2])

number_proportion <- paste0(number, " (", round(proportion*100,2), "\\%)")

length_of_stay_num <- table(df1$length_of_stay)
length_of_stay_prop <- round(prop.table(length_of_stay_num)*100,2)
length_of_stay <- paste0(length_of_stay_num, " (", length_of_stay_prop, "\\%)")

# exposures
mme <- paste0(round(median(df1$exposure_mean_daily_dose_mme),1)," (",
              round(quantile(df1$exposure_mean_daily_dose_mme, 0.25),1),", ",
              round(quantile(df1$exposure_mean_daily_dose_mme, 0.75),1),")")
days <- paste0(median(df1$exposure_days_supply)," (",
               quantile(df1$exposure_days_supply, 0.25),", ",
               quantile(df1$exposure_days_supply, 0.75),")")


calculate_KM <- function(data, outcome_name, censor_name) {
  m <- c(sum(data[[paste0(outcome_name, 1)]], na.rm=T),
         sum(data[[paste0(outcome_name, 2)]], na.rm=T) - sum(data[[paste0(outcome_name, 1)]], na.rm=T),
         sum(data[[paste0(outcome_name, 3)]], na.rm=T) - sum(data[[paste0(outcome_name, 2)]], na.rm=T),
         sum(data[[paste0(outcome_name, 4)]], na.rm=T) - sum(data[[paste0(outcome_name, 3)]], na.rm=T))
  n <- c(nrow(data),
         sum(data[[paste0(censor_name, 1)]], na.rm=T) - m[1],
         sum(data[[paste0(censor_name, 2)]], na.rm=T) - m[1] - m[2],
         sum(data[[paste0(censor_name, 3)]], na.rm=T) - m[1] - m[2] - m[3])
  
  hazard <- m / n
  incidence <- cumsum(m)
  survival_reciprocal <- round((1-(cumprod(1 - hazard)))*100,2)
  
  
  stopifnot(all(m <= n, na.rm = TRUE))
  stopifnot(all(n >= 0))
  
  return(list(number_at_risk = n,
              survival = paste0(incidence," (", survival_reciprocal,"\\%)")))
}

S_hillary <- calculate_KM(df1, "oud_hillary_period_", "cens_hillary_period_")
S_moud <- calculate_KM(df1, "moud_period_", "cens_moud_period_")
S_oud <- calculate_KM(df1, "oud_period_", "cens_oud_period_")

num_remaining_other <- c(rep(NA, 37), 
                         c(paste0("(n=", S_hillary$number_at_risk,")\\textsuperscript{a}")),
                         NA,
                         c(paste0("(n=", S_moud$number_at_risk,")\\textsuperscript{a}")),
                         NA,
                         c(paste0("(n=", S_oud$number_at_risk,")\\textsuperscript{a}")))

outcomes <- c(NA,
              c(paste0(S_hillary$survival, "\\textsuperscript{b}")),
              NA,
              c(paste0(S_moud$survival, "\\textsuperscript{b}")),
              NA,
              c(paste0(S_oud$survival, "\\textsuperscript{b}"))
)

other <- c(age, number_proportion, NA, length_of_stay, mme, days, outcomes)



# C-SECTION

age <- paste0(median(df2$age_enrollment)," (",
              quantile(df2$age_enrollment, 0.25),", ",
              quantile(df2$age_enrollment, 0.75),")")

number <- sapply(df2[,5:29], function(x) sum(x))
proportion <- sapply(df2[,5:29], function(x) prop.table(table(x))[2])

number_proportion <- paste0(number, " (", round(proportion*100,2), "\\%)")

# exposures
mme <- paste0(round(median(df2$exposure_mean_daily_dose_mme),1)," (",
              round(quantile(df2$exposure_mean_daily_dose_mme, 0.25),1),", ",
              round(quantile(df2$exposure_mean_daily_dose_mme, 0.75),1),")")
days <- paste0(median(df2$exposure_days_supply)," (",
               quantile(df2$exposure_days_supply, 0.25),", ",
               quantile(df2$exposure_days_supply, 0.75),")")

S_hillary <- calculate_KM(df2, "oud_hillary_period_", "cens_hillary_period_")
S_moud <- calculate_KM(df2, "moud_period_", "cens_moud_period_")
S_oud <- calculate_KM(df2, "oud_period_", "cens_oud_period_")

num_remaining_cs <- c(rep(NA, 37), 
                      c(paste0("(n=", S_hillary$number_at_risk,")\\textsuperscript{a}")),
                      NA,
                      c(paste0("(n=", S_moud$number_at_risk,")\\textsuperscript{a}")),
                      NA,
                      c(paste0("(n=", S_oud$number_at_risk,")\\textsuperscript{a}")))

outcomes <- c(NA,
              c(paste0(S_hillary$survival, "\\textsuperscript{b}")),
              NA,
              c(paste0(S_moud$survival, "\\textsuperscript{b}")),
              NA,
              c(paste0(S_oud$survival, "\\textsuperscript{b}"))
)

c_section <- c(age, number_proportion, rep(NA,8), mme, days, outcomes)

table_one <- data.frame(
  demographics = demographics,
  num_remaining_other = num_remaining_other,
  other = other,
  num_remaining_cs = num_remaining_cs,
  c_section = c_section
)

write.csv(table_one, "~/medicaid/post_surgery_opioid_use/output/table_one_sensitivity_20250811.csv", row.names = F)

