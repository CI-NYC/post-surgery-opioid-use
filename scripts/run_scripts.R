library(callr)
library(future)
library(future.apply)

setwd("~/medicaid/post_surgery_opioid_use/scripts")


job_groups <- list(
  # group0 = paste0("06_analysis/06_00_lmtp_runs/", c("lmtp_no-cs_hillary.R","lmtp_no-cs_oud.R","lmtp_no-cs_moud.R","lmtp_only-cs_hillary.R")),
  group1 = c(paste0("06_analysis/06_00_lmtp_runs/", c("lmtp_only-cs_moud.R","lmtp_only-cs_oud.R")),
             paste0("08_sensitivity_analysis/sensitivity_analysis_runs/", c("lmtp_no-cs_hillary.R"))),
  group2 = paste0("08_sensitivity_analysis/sensitivity_analysis_runs/", c("lmtp_no-cs_moud.R","lmtp_no-cs_oud.R","lmtp_only-cs_hillary.R")),
  group3 = paste0("08_sensitivity_analysis/sensitivity_analysis_runs/", c("lmtp_only-cs_moud.R","lmtp_only-cs_oud.R")),
  group4 = c("06_analysis/06_02_survival_plots.R","08_sensitivity_analysis/06_survival_plots_sensitivity.R")
)

# 1a. Flatten all paths
all_paths <- unlist(job_groups, use.names = FALSE)

# 1b. Test existence
exists_vec <- file.exists(all_paths)

# 1c. Report
if (all(exists_vec)) {
  message("✅ All files exist.")
} else {
  missing <- all_paths[!exists_vec]
  warning("❌ Missing files:\n", paste(missing, collapse = "\n"))
}

plan(multisession, workers = 4)

run_jobs_future <- function(groups, log_dir = "logs") {
  # make sure log directory exists
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  for (grp in names(groups)) {
    scripts <- groups[[grp]]
    message("Launching group: ", grp)
    
    # launch all scripts in parallel futures
    futures <- future_lapply(scripts, function(file) {
      # build a log-file name, e.g. "logs/group2_07_finalize_treatment_dts.R.log"
      script_name <- tools::file_path_sans_ext(basename(file))
      log_file     <- file.path(log_dir, paste0(grp, "_", script_name, ".log"))
      
      callr::rscript(
        script = file,
        stdout = log_file,
        stderr = log_file
      )
      TRUE  # return TRUE on success
    }, future.seed = TRUE)
    
    message("Group ", grp, " completed successfully")
  }
}

tryCatch(
  run_jobs_future(job_groups),
  error = function(e) {
    message("Workflow halted: ", conditionMessage(e))
    quit(save = "no", status = 1, runLast = FALSE)
  }
)