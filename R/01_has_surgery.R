# -------------------------------------
# Script: has_surgery
# Author: Anton Hung
# Purpose: Identify which individuals have an eligible surgery claim code
# Notes:
# -------------------------------------

# cohort data
# cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds")
# cohort <- cohort[, "BENE_ID"]

# claims data
src_root <- "/mnt/processed-data/disability"
# drv_root <- "/home/amh2389/medicaid"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# surgery claims
variable = "Surgery"
codes <- read_yaml("/home/amh2389/medicaid/poss/R/surgery_codes.yml")
codes <- c(names(codes[[variable]]$CPT),
           names(codes[[variable]]$ICD10),
           names(codes[[variable]]$CCS))

# Filter OTL to claims codes
claims_vars <- c("BENE_ID", "CLM_ID", "MSIS_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |>
  filter(LINE_PRCDR_CD %in% codes) |>
  filter(!is.na(BENE_ID)) |>
  # mutate(LINE_SRVC_END_DT = case_when(is.na(LINE_SRVC_END_DT) ~ LINE_SRVC_BGN_DT, TRUE ~ LINE_SRVC_END_DT)) |>
  collect()

claims <- unique(claims)

saveRDS(claims, "/mnt/general-data/disability/post_surgery_opioid_use/intermediate/surgery_claims.rds")
