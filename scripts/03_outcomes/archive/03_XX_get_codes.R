# -------------------------------------
# Script:
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------

# otl and rxl data for finding opioid claims
src_root <- "/mnt/processed-data/disability"

# # Read in OTL (Other services line) 
# files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
# parquet_files <- grep("\\.parquet$", files, value = TRUE)
# otl <- open_dataset(file.path(src_root, parquet_files))

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFRXL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxl <- open_dataset(file.path(src_root, parquet_files))


cohort <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/intermediate/first_surgeries.rds")
setDT(cohort)


rxl_vars <- c("BENE_ID", "CLM_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
  filter(BENE_ID %in% cohort$BENE_ID) |>
  # filter(NDC %in% op$NDC) |>
  collect() |> 
  rename(OPIOID_ID = CLM_ID) |>
  as.data.table() |>
  left_join(cohort[,.(BENE_ID, discharge_dt)]) |>
  filter(RX_FILL_DT %within% interval(discharge_dt, discharge_dt %m+% months(24)))

rxl2 <- rxl |>
  select(NDC) |>
  unique()

write.csv(rxl2, "/mnt/general-data/disability/post_surgery_opioid_use/outcomes/moud/elig_rxl_codes.csv", row.names = F)
