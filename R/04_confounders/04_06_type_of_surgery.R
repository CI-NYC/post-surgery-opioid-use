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

surgery_type <- data.frame(
  PRCDR_CD = codes,
  surgery_major = c(rep(0,50), rep(1,length(codes)-50))
)

cohort$PRCDR_CD <- sapply(cohort$PRCDR_CD, first)

cohort <- cohort |>
  left_join(surgery_type, by="PRCDR_CD")

# expecting an 80 - 20 percent split between minor/major. But it's nearly 100% minor in my cohort

cohort <- cohort |>
  select("BENE_ID","surgery_major")
saveRDS(cohort, "/mnt/general-data/disability/post_surgery_opioid_use/confounders/confound_major_surgery.rds")


# Check surgeries from Sun paper

codes <- c('27447',# Total Knee Arthroplasty # major
'27130',#: Total Hip Arthroplasty #
'47600',#: Open Cholecystectomy
'47605',#: Open Cholecystectomy
'47610',#: Open Cholecystectomy
'44979',#: Laparoscopic Appendectomy
'44950',#: Open Appendectomy
'44960',#: Open Appendectomy
'59510',#: Cesarean Section
'59514',#: Cesarean Section
'59515',#: Cesarean Section
'31237',#: FESS
'31240',#: FESS
'31254',#: FESS
'31255',#: FESS
'31256',#: FESS
'31267',#: FESS
'31276',#: FESS
'31287',#: FESS
'31288',#: FESS
'66982',#: Cataract Surgery
'66983',#: Cataract Surgery
'66984',#: Cataract Surgery
'52612',#: TURP
'52614',#: TURP
'19301',#: Simple Mastectomy
'19302',#: Simple Mastectomy
'19303',#: Simple Mastectomy
'19180'#: Simple Mastectomy
)

prcdr_cd <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds") |>
  select(BENE_ID, PRCDR_CD)

sex_m <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/combined_df.rds") |> select(BENE_ID, SEX_M)

cohort <- prcdr_cd |>
  left_join(sex_m) |>
  filter(PRCDR_CD %in% codes) |>
  select(BENE_ID, SEX_M, PRCDR_CD) |>
  mutate(PRCDR_CD = as.character(unlist(PRCDR_CD)))
