# -------------------------------------
# Script: make_table_one
# Author: Anton Hung
# Date: June 24 2024
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)

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
                  "Minor"
)

age <- paste0(median(df1$age_enrollment),
              " (",
              quantile(df1$age_enrollment, 0.25),
              ", ",
              quantile(df1$age_enrollment, 0.75),
              ")")

number <- sapply(df1[,6:27], function(x) sum(x))
proportion <- sapply(df1[,6:27], function(x) prop.table(table(x))[2])

number_proportion <- paste0(number, " (", round(proportion*100,2), "%)")

other <- c(age, number_proportion)



# table_one <- data.frame(
#   demographics = demographics,
#   value = value
# )


# write.csv(table_one, "~/medicaid/post_surgery_opioid_use/output/table_one_other.csv")




age <- paste0(median(df2$age_enrollment),
              " (",
              quantile(df2$age_enrollment, 0.25),
              ", ",
              quantile(df2$age_enrollment, 0.75),
              ")")

number <- sapply(df2[,6:27], function(x) sum(x))
proportion <- sapply(df2[,6:27], function(x) prop.table(table(x))[2])

number_proportion <- paste0(number, " (", round(proportion*100,2), "%)")

c_section <- c(age, number_proportion)


table_one <- data.frame(
  demographics = demographics,
  other = other,
  c_section = c_section
)


write.csv(table_one, "~/medicaid/post_surgery_opioid_use/output/table_one.csv", row.names = F)

