# -------------------------------------
# Script: make_substance_yaml
# Author: Anton Hung 2024-05-30
# Purpose:
# Notes:
# -------------------------------------


definitions_table <- read.delim(file = "~/medicaid/post_surgery_opioid_use/input/2018_code_descriptions/icd10cm_codes_2018.txt", header=F)

definitions_table <- separate(definitions_table, V1, into = c("ICD10", "definition"), sep = "^\\S*\\K\\s+")

alcohol_codes <- definitions_table |>
  filter(grepl("^F10[1-2]", ICD10))

substance_codes <- definitions_table |>
  filter(grepl("^F1[2-9][1-2]", ICD10)) |>
  filter(!grepl("^F17", ICD10))

smoking_codes <- definitions_table |>
  filter(grepl("^F17", ICD10) | ICD10 == "Z87891")

write.table(alcohol_codes, "~/medicaid/post_surgery_opioid_use/input/alcohol_codes.txt", row.names=F, sep = ": ")
write.table(substance_codes, "~/medicaid/post_surgery_opioid_use/input/substance_codes.txt", row.names=F, sep = ": ")
write.table(smoking_codes, "~/medicaid/post_surgery_opioid_use/input/smoking_codes.txt", row.names=F, sep = ": ")

