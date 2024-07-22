# -------------------------------------
# Script: make_table_one
# Author: Anton Hung
# Date: June 24 2024
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)

df1 <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/df_non_c_section.rds")
# 5:22

df2 <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/df_only_c_section.rds") |>
  filter(!SEX_M == 1)
# 5:20

df1 <- df1 |>
  mutate(sex = NA, .before = SEX_M) |>
  mutate(SEX_F = 1 - SEX_M, .after=SEX_M) |>
  mutate(psychiatric_conditions = NA, .before = has_anxiety) |>
  mutate(substance_abuse = NA, .before = has_alcohol_abuse) |>
  mutate(pain = NA, .before = has_pain_back) |>
  mutate(surgery_type = NA, .before = surgery_major) |>
  mutate(surgery_minor = 1 - surgery_major, .after=surgery_major) |>
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
  as.data.table()
  
calc_numremaining <- function(outcome, uncensored){
  outcome <- outcome[which(uncensored == 1 | outcome == 1)]
  n <- length(outcome)
  
  # return(n)
  return(n)
}

c(NA,
  calc_numremaining(df1$Y2_1, df1$C_1),
  calc_numremaining(df1$Y2_2, df1$C_2),
  calc_numremaining(df1$Y2_3, df1$C_3),
  calc_numremaining(df1$Y2_4, df1$C_4),
  NA,
  calc_numremaining(df1$Y4_1, df1$C_1),
  calc_numremaining(df1$Y4_2, df1$C_2),
  calc_numremaining(df1$Y4_3, df1$C_3),
  calc_numremaining(df1$Y4_4, df1$C_4),
  NA,
  calc_numremaining(df1$Y3_1, df1$C_1),
  calc_numremaining(df1$Y3_2, df1$C_2),
  calc_numremaining(df1$Y3_3, df1$C_3),
  calc_numremaining(df1$Y3_4, df1$C_4),
  NA,
  calc_numremaining(df1$C_1, df1$C_1),
  calc_numremaining(df1$C_2, df1$C_2),
  calc_numremaining(df1$C_3, df1$C_3),
  calc_numremaining(df1$C_4, df1$C_4))

demographics <- c("Age",
                  "Sex:",
                  "Male",
                  "Female",
                  "Psychiatric Conditions:",
                  "Anxiety",
                  "Depression",
                  "Bipolar",
                  "Substance Abuse Disorders:",
                  "Alcohol",
                  "Other substances",
                  "Nicotine",
                  "Pain Disorders:",
                  "Back pain",
                  "Neck pain",
                  "Arthritis",
                  "Neurological pain",
                  "Headache",
                  "Miscellaneous pain",
                  "Unspecified pain, back or neck",
                  "Surgery Type:",
                  "Major",
                  "Minor",
                  "Dose (MME)",
                  "Days of continous use",
                  "OUD (ICD only):",
                  "at 6 months (n=162 985)",
                  "at 12 months (n=126 857)",
                  "at 18 months (n=94 705)",
                  "at 24 months (n=66 255)",
                  "MOUD:",
                  "at 6 months (n=162 907)",
                  "at 12 months (n=126 597)",
                  "at 18 months (n=94 155)",
                  "at 24 months (n=65 342)",
                  "OUD (comprehensive):",
                  "at 6 months (n=162 998)",
                  "at 12 months (n=126 903)",
                  "at 18 months (n=94 793",
                  "at 24 months (n=66 388)",
                  "Remained enrolled:",
                  "at 6 months",
                  "at 12 months",
                  "at 18 months",
                  "at 24 months"
)

age <- paste0(median(df1$age_enrollment)," (",
              quantile(df1$age_enrollment, 0.25),", ",
              quantile(df1$age_enrollment, 0.75),")")

number <- sapply(df1[,6:27], function(x) sum(x))
proportion <- sapply(df1[,6:27], function(x) prop.table(table(x))[2])

number_proportion <- paste0(number, " (", round(proportion*100,2), "%)")

# exposures
mme <- paste0(round(median(df1$mean_daily_dose_mme),1)," (",
              round(quantile(df1$mean_daily_dose_mme, 0.25),1),", ",
              round(quantile(df1$mean_daily_dose_mme, 0.75),1),")")
days <- paste0(median(df1$days_of_continuous_use)," (",
              quantile(df1$days_of_continuous_use, 0.25),", ",
              quantile(df1$days_of_continuous_use, 0.75),")")

calc_cumincidence <- function(outcome, uncensored){
  outcome <- outcome[which(uncensored == 1 | outcome == 1)]
  n <- length(outcome)
  
  # return(n)
  return(paste0(sum(outcome), " (",
                round(sum(outcome)/n * 100, 2), "%)"))
}

# outcomes
outcomes <- c(NA,
  calc_cumincidence(df1$Y2_1, df1$C_1),
  calc_cumincidence(df1$Y2_2, df1$C_2),
  calc_cumincidence(df1$Y2_3, df1$C_3),
  calc_cumincidence(df1$Y2_4, df1$C_4),
  NA,
  calc_cumincidence(df1$Y4_1, df1$C_1),
  calc_cumincidence(df1$Y4_2, df1$C_2),
  calc_cumincidence(df1$Y4_3, df1$C_3),
  calc_cumincidence(df1$Y4_4, df1$C_4),
  NA,
  calc_cumincidence(df1$Y3_1, df1$C_1),
  calc_cumincidence(df1$Y3_2, df1$C_2),
  calc_cumincidence(df1$Y3_3, df1$C_3),
  calc_cumincidence(df1$Y3_4, df1$C_4),
  NA,
  calc_cumincidence(df1$C_1, df1$C_1),
  calc_cumincidence(df1$C_2, df1$C_2),
  calc_cumincidence(df1$C_3, df1$C_3),
  calc_cumincidence(df1$C_4, df1$C_4))


other <- c(age, number_proportion, mme, days, outcomes)



# table_one <- data.frame(
#   demographics = demographics,
#   value = value
# )


# write.csv(table_one, "~/medicaid/post_surgery_opioid_use/output/table_one_other.csv")




age <- paste0(median(df2$age_enrollment)," (",
              quantile(df2$age_enrollment, 0.25),", ",
              quantile(df2$age_enrollment, 0.75),")")

number <- sapply(df2[,6:27], function(x) sum(x))
proportion <- sapply(df2[,6:27], function(x) prop.table(table(x))[2])

number_proportion <- paste0(number, " (", round(proportion*100,2), "%)")

# exposures
mme <- paste0(round(median(df2$mean_daily_dose_mme),1)," (",
              round(quantile(df2$mean_daily_dose_mme, 0.25),1),", ",
              round(quantile(df2$mean_daily_dose_mme, 0.75),1),")")
days <- paste0(median(df2$days_of_continuous_use)," (",
               quantile(df2$days_of_continuous_use, 0.25),", ",
               quantile(df2$days_of_continuous_use, 0.75),")")

# outcomes
outcomes <- c(NA,
              calc_cumincidence(df2$Y2_1, df2$C_1),
              calc_cumincidence(df2$Y2_2, df2$C_2),
              calc_cumincidence(df2$Y2_3, df2$C_3),
              calc_cumincidence(df2$Y2_4, df2$C_4),
              NA,
              calc_cumincidence(df2$Y4_1, df2$C_1),
              calc_cumincidence(df2$Y4_2, df2$C_2),
              calc_cumincidence(df2$Y4_3, df2$C_3),
              calc_cumincidence(df2$Y4_4, df2$C_4),
              NA,
              calc_cumincidence(df2$Y3_1, df2$C_1),
              calc_cumincidence(df2$Y3_2, df2$C_2),
              calc_cumincidence(df2$Y3_3, df2$C_3),
              calc_cumincidence(df2$Y3_4, df2$C_4),
              NA,
              calc_cumincidence(df2$C_1, df2$C_1),
              calc_cumincidence(df2$C_2, df2$C_2),
              calc_cumincidence(df2$C_3, df2$C_3),
              calc_cumincidence(df2$C_4, df2$C_4))

c_section <- c(age, number_proportion, mme, days, outcomes)

placeholder <- c(rep(NA, 25),
                 NA,
                 calc_numremaining(df2$Y2_1, df2$C_1),
                 calc_numremaining(df2$Y2_2, df2$C_2),
                 calc_numremaining(df2$Y2_3, df2$C_3),
                 calc_numremaining(df2$Y2_4, df2$C_4),
                 NA,
                 calc_numremaining(df2$Y4_1, df2$C_1),
                 calc_numremaining(df2$Y4_2, df2$C_2),
                 calc_numremaining(df2$Y4_3, df2$C_3),
                 calc_numremaining(df2$Y4_4, df2$C_4),
                 NA,
                 calc_numremaining(df2$Y3_1, df2$C_1),
                 calc_numremaining(df2$Y3_2, df2$C_2),
                 calc_numremaining(df2$Y3_3, df2$C_3),
                 calc_numremaining(df2$Y3_4, df2$C_4),
                 rep(NA, 5))

table_one <- data.frame(
  demographics = demographics,
  other = other,
  placeholder = placeholder,
  c_section = c_section
)


write.csv(table_one, "~/medicaid/post_surgery_opioid_use/output/table_one_uncertain.csv", row.names = F)

