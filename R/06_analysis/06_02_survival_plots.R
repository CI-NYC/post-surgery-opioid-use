# -------------------------------------
# Script: survival_plots
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(purrr)
library(lmtp)

result_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/analysis/plots"

for (Y in c("Y1", "Y2","Y3","Y4","Y5")){
  
  lmtp_object <- readRDS(paste0("/mnt/general-data/disability/post_surgery_opioid_use/analysis/lmtp_result_",Y,".rds"))
  
  
  results_observed <- map_dfr(lmtp_object$observed, tidy, .id = "t") |> 
    mutate(estimate = ifelse(t == 1, estimate, 1 - estimate), 
           t = as.numeric(t),
           type = "observed") |>
    add_row(t = 0, estimator = "TMLE", estimate = 0, std.error = 0, conf.low = 0, conf.high = 0, type = 'observed')
  
  results_intervention <- map_dfr(lmtp_object$intervention, tidy, .id = "t") |> 
    mutate(estimate = ifelse(t == 1, estimate, 1 - estimate), 
           t = as.numeric(t),
           type = "intervention") |>
    add_row(t = 0, estimator = "TMLE", estimate = 0, std.error = 0, conf.low = 0, conf.high = 0, type = 'intervention')
  
  
  combined_results <- rbind(results_observed,results_intervention)
  
  p <- ggplot(combined_results, aes(x = t, y = estimate, colour = type)) + 
    geom_step() + 
    labs(x = "6-month period", y = "cummulative incidence") + 
    scale_x_continuous(limits = c(0, 4), 
                       n.breaks = 4, 
                       expand = c(0.01, 0)) +
    theme_classic()
  p
  ggsave(file = file.path(result_dir, paste0("surv_plot_", Y, ".pdf")), width = 7, height = 7)
  
}

