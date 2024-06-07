#--------------------
## Title: conversions
# Author: Anton Hung
# Date: April 3rd
# Purpose: Converting ICD 9 codes to ICD 10
#--------------------

library(dplyr)
library(data.table)
library(tidyverse)

# starting with 31 ICD 9 codes
ICD9_codes <- c(4498,4464,5351,4467,4495,5362,5371,5383,4581,4438,4468,6831,6841,
                6851,4382,4573,4582,5372,5361,5384,5369,4583,4439,5363,4466,6839,
                6849,689,5375,5359,5380)


conversion_table <- read.table(file = "~/medicaid/post_surgery_opioid_use/input/2018icd10pcsgeneralequivalencemappings/gem_i9pcs.txt")

colnames(conversion_table) <- c("ICD9", "ICD10", "flag")

ICD10_codes <- conversion_table |>
  filter(ICD9 %in% ICD9_codes) |>
  select(ICD10) |>
  distinct()


### Definitions

definitions_table <- read.delim(file = "~/medicaid/post_surgery_opioid_use/input/2018_icd10pcs_codes_file/icd10pcs_codes_2018.txt", header=F)

definitions_table <- separate(definitions_table, V1, into = c("ICD10", "definition"), sep = "^\\S*\\K\\s+")


ICD10_table <- ICD10_codes |>
  left_join(definitions_table, by="ICD10") |>
  distinct(ICD10, definition)

write.table(ICD10_table, "/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q1_2024/lisa/ICD conversions/ICD10_codes.csv", row.names=F, sep = ": ")



### Grouping codes together


# Split based on comma or the word "to"
ICD10_table$group <- sapply(strsplit(ICD10_table$definition, "(,|\\bwith\\b)"), "[", 1) |>
  trimws()
names(ICD10_table)[1] <- "code"

# ICD10_table$group2 <- c(rep("Parathyroid Gland", 6),
#                         rep("Stomach", 27),
#                         rep("Esophagogastric Junction", 13),
#                         rep("Stomach", 2),
#                         rep("Introduction of Other Therapeutic Substance into Upper GI", 2),
#                         rep("Right Large Intestine", 3),
#                         rep("Ascending Colon", 1),
#                         rep("Large Intestine", 4),
#                         rep("Abdominal Wall", 9),
#                         rep("Diaphragm",))


CPT_table <- read.delim(file = "~/medicaid/post_surgery_opioid_use/input/cpt_codes.tsv")
CPT_table$group <- c("Carpal tunnel release",
                     "Parathyroidectomy",
                     "Transurethral procedure on the prostate",
                     "Cholecystectomy",
                     "Cholecystectomy",
                     "Cholecystectomy",
                     "Appendectomy",
                     "Ligation, division and/or excision of varicose vein cluster(s)",
                     "Ligation of hemorrhoids",
                     "Transurethral procedure on the prostate",
                     "Removal of thyroid",
                     "Ligation of hemorrhoids",
                     "Ligation of hemorrhoids",
                     rep("Hemorrhoidectomy", 7),
                     "Transurethral procedure on the prostate")

code_groups <- ICD10_table |> 
  rbind(CPT_table)

write.csv(code_groups, "~/medicaid/post_surgery_opioid_use/input/code_groups.csv", row.names = F)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/confounders/confound_major_surgery_specific.rds")
surgery_counts <- as.data.frame(table(cohort$group)) |>
  rename(group = Var1)

code_groups <- read.csv("~/medicaid/post_surgery_opioid_use/input/code_groups.csv") |>
  mutate(code_type = c(rep("ICD10", 90),
                       rep("CPT", 21))) |>
  arrange(group) |>
  select(code_type, code, definition, group)

code_groups <- code_groups |>
  left_join(surgery_counts, by="group")


write.csv(code_groups, "~/medicaid/post_surgery_opioid_use/input/code_groups.csv", row.names = F)





### ICD-9 to ICD-10 table

ICD10_codes <- conversion_table |>
  filter(ICD9 %in% ICD9_codes) |>
  group_by(ICD9) |>
  mutate(code_num = row_number()) |>
  select(ICD9, ICD10, code_num) |>
  pivot_wider(names_from = code_num, values_from = ICD10, names_prefix="ICD10 #")

write.csv(ICD10_codes, "~/medicaid/post_surgery_opioid_use/input/ICD10_translations.csv", row.names = F)



### Better groups
code_groups <- read.csv("~/medicaid/post_surgery_opioid_use/input/code_groups.csv") |>
  arrange(code) |>
  select(code_type, code, definition) |>
  mutate(group = c(rep("Procedure on Diaphragm",15),
                   rep("Bypass Stomach",24),
                   rep("Procedure on Stomach",4),
                   rep("Resection of Large Intestine",8),
                   rep("Restriction of Esophagogastric Junction",14),
                   rep("Excision of Parathyroid Gland",6),
                   rep("Resection of Uterus or Cervix",8),
                   rep("Repair Abdominal Wall",9),
                   rep("Introduction of Other Therapeutic Substance into Upper GI",2),
                   "Ligation of varicose vein clusters",
                   "Appendectomy",
                   rep("Procedure on Hemorrhoids",10),
                   rep("Cholecystectomy",3),
                   rep("Transurethral procedure on the prostate",3),
                   rep("Removal of thyroid/parathyroid",2),
                   "Carpal tunnel release"))
