#--------------------
## Title: conversions
# Author: Anton Hung
# Date: April 3rd
# Purpose: Converting ICD 9 codes to ICD 10
#--------------------

library(dplyr)
library(data.table)
library(tidyr)

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

definitions_table <- read.delim(file = "/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q1_2024/lisa/ICD conversions/2018_icd10pcs_codes_file/icd10pcs_codes_2018.txt", header=F)

definitions_table <- separate(definitions_table, V1, into = c("ICD10", "definition"), sep = "^\\S*\\K\\s+")


ICD10_table <- ICD10_codes |>
  left_join(definitions_table, by="ICD10") |>
  distinct(ICD10, definition)

write.table(ICD10_table, "/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q1_2024/lisa/ICD conversions/ICD10_codes.csv", row.names=F, sep = ": ")


### making an ICD-9 to ICD-10 mapping table for later reference

# 
# surgery_type <- data.frame(
#   ICD9 <- ICD9_codes,
#   type <- c(rep("minor",21), rep("major",21))
# )
# df <- data.frame(
#   Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
#   Age = c(23, 41, 32, 58, 26)
# )
# 
# 
# conversion_table <- conversion_table |>
#   filter(ICD9 %in% ICD9_codes) |>
#   select(ICD9, ICD10)
# 
# 
# 
# 
