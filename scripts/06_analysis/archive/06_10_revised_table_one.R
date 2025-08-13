# -------------------------------------
# Script: make_table_one
# Author: Anton Hung
# Date: June 24 2024
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)
library(survival)

df1 <- load_data("df_non_c_section.fst", file.path(drv_root, "final"))
# 5:22

df2 <- load_data("df_only_c_section.fst", file.path(drv_root, "final"))
# 5:20

df1_hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/hillary_KM.rds") |>
  filter(BENE_ID %in% df1$BENE_ID)
df1_moud <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/moud_KM.rds") |>
  filter(BENE_ID %in% df1$BENE_ID)
df1_oud <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/oud_KM.rds") |>
  filter(BENE_ID %in% df1$BENE_ID)

df2_hillary <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/hillary_KM.rds") |>
  filter(BENE_ID %in% df2$BENE_ID)
df2_moud <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/moud_KM.rds") |>
  filter(BENE_ID %in% df2$BENE_ID)
df2_oud <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/oud_KM.rds") |>
  filter(BENE_ID %in% df2$BENE_ID)

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

number <- sapply(df1[,6:30], function(x) sum(x))
proportion <- sapply(df1[,6:30], function(x) prop.table(table(x))[2])

number_proportion <- paste0(number, " (", round(proportion*100,2), "\\%)")

length_of_stay_num <- table(df1$length_of_stay)
length_of_stay_prop <- round(prop.table(length_of_stay_num)*100,2)
length_of_stay <- paste0(length_of_stay_num, " (", length_of_stay_prop, "\\%)")

# exposures
mme <- paste0(round(median(df1$mean_daily_dose_mme),1)," (",
              round(quantile(df1$mean_daily_dose_mme, 0.25),1),", ",
              round(quantile(df1$mean_daily_dose_mme, 0.75),1),")")
days <- paste0(median(df1$days_supplied)," (",
               quantile(df1$days_supplied, 0.25),", ",
               quantile(df1$days_supplied, 0.75),")")



new_cases <- c()
cum_inc <- c()
num_remaining_other <- c(rep(NA, 36))

list_of_dfs <- list(df1_hillary, df1_moud, df1_oud)
for (i in seq_along(list_of_dfs)) {
  df <- list_of_dfs[[i]]
  
  surv_obj <- Surv(df$time, df$status)
  fit <- survfit(surv_obj ~ 1)
  
  new_cases <- c(new_cases, NA, cumsum(fit$n.event[1:4]))
  cum_inc <- c(cum_inc, NA, 1-fit$surv[1:4])
  num_remaining_other <- c(num_remaining_other, NA, paste0("(n=",fit$n.risk[1:4],")\\textsuperscript{1}"))
}
outcomes <- paste0(new_cases," (",round(cum_inc*100,2),"\\%)\\textsuperscript{2}")
other <- c(age, number_proportion, NA, length_of_stay, mme, days, outcomes)





# C-SECTION

age <- paste0(median(df2$age_enrollment)," (",
              quantile(df2$age_enrollment, 0.25),", ",
              quantile(df2$age_enrollment, 0.75),")")

number <- sapply(df2[,6:30], function(x) sum(x))
proportion <- sapply(df2[,6:30], function(x) prop.table(table(x))[2])

number_proportion <- paste0(number, " (", round(proportion*100,2), "\\%)")

# exposures
mme <- paste0(round(median(df2$mean_daily_dose_mme),1)," (",
              round(quantile(df2$mean_daily_dose_mme, 0.25),1),", ",
              round(quantile(df2$mean_daily_dose_mme, 0.75),1),")")
days <- paste0(median(df2$days_supplied)," (",
               quantile(df2$days_supplied, 0.25),", ",
               quantile(df2$days_supplied, 0.75),")")

new_cases <- c()
cum_inc <- c()
num_remaining_cs <- c(rep(NA, 36))

list_of_dfs <- list(df2_hillary, df2_moud, df2_oud)
for (i in seq_along(list_of_dfs)) {
  df <- list_of_dfs[[i]]
  
  surv_obj <- Surv(df$time, df$status)
  fit <- survfit(surv_obj ~ 1)
  
  new_cases <- c(new_cases, NA, cumsum(fit$n.event[1:4]))
  cum_inc <- c(cum_inc, NA, 1-fit$surv[1:4])
  num_remaining_cs <- c(num_remaining_cs, NA, paste0("(n=",fit$n.risk[1:4],")\\textsuperscript{1}"))
}

outcomes <- paste0(new_cases," (",round(cum_inc*100,2),"\\%)\\textsuperscript{2}")

c_section <- c(age, number_proportion, rep(NA,8), mme, days, outcomes)

table_one <- data.frame(
  demographics = demographics,
  num_remaining_other = num_remaining_other,
  other = other,
  num_remaining_cs = num_remaining_cs,
  c_section = c_section
)


write.csv(table_one, "~/medicaid/post_surgery_opioid_use/output/table_one_updated_mme.csv", row.names = F)

