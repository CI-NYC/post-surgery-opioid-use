# -------------------------------------
# Script: 04_06_type_of_surgery
# Author: Anton Hung 2024-05-30
# Purpose:
# Notes:
# -------------------------------------
library(yaml)
library(dplyr)

cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")

# surgery claims
variable = "Surgery"
codes <- read_yaml("/home/amh2389/medicaid/post_surgery_opioid_use/input/surgery_codes.yml")
codes <- c(names(codes[[variable]]$CPT),
           names(codes[[variable]]$ICD10))

surgery_type <- read.csv("~/medicaid/post_surgery_opioid_use/input/code_groups.csv")
colnames(surgery_type) <- c("code_type", "PRCDR_CD", "definition", "group")


# THIS LINE MIGHT NEED TO BE REVISED
cohort$PRCDR_CD <- sapply(cohort$PRCDR_CD, first)


cohort <- cohort |>
  left_join(surgery_type, by="PRCDR_CD")

# expecting an 80 - 20 percent split between minor/major. But it's nearly 100% minor in my cohort

# cohort <- cohort |>
#   select(BENE_ID, group)
# saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confound_major_surgery_specific.rds")

cohort <- cohort |>
  select(BENE_ID, group) |>
  mutate(surgery_diaphragm = +(group == "Procedure on Diaphragm"),
         surgery_bypass_stomach = +(group == "Bypass Stomach"),
         surgery_stomach_procedure = +(group == "Procedure on Stomach"),
         surgery_resection_LI = +(group == "Resection of Large Intestine"),
         surgery_esophagogastric = +(group == "Restriction of Esophagogastric Junction"),
         surgery_excision_parathyroid = +(group == "Excision of Parathyroid Gland"),
         surgery_hysterectomy = +(group == "Resection of Uterus or Cervix"),
         surgery_abdominal = +(group == "Repair Abdominal Wall"),
         surgery_introduction_into_GI = +(group == "Introduction of Other Therapeutic Substance into Upper GI"),
         surgery_ligation_varicose_vein = +(group == "Ligation of varicose vein clusters"),
         surgery_appendectomy = +(group == "Appendectomy"),
         surgery_hemorrhoids = +(group == "Procedure on Hemorrhoids"),
         surgery_cholecystectomy = +(group == "Cholecystectomy"),
         surgery_transurethral_prostate = +(group == "Transurethral procedure on the prostate"),
         surgery_removal_parathyroid_thyroid = +(group == "Removal of thyroid/parathyroid"),
         surgery_carpal_tunnel_release = +(group == "Carpal tunnel release")) |>
  select(-group)





saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confounder_pain.rds")
