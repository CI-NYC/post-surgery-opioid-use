# -------------------------------------
# Script: survival_plots
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(purrr)
library(lmtp)

load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/analysis/ver3/"
result_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/analysis/ver3/plots"

outcome_codes <- data.frame(Y = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6"),
                            name = c("Overdose","OUD","MOUD_met","MOUD_nal","MOUD_bup", "MOUD"))

combine_intervention_observed <- function(outcome) {
  # load lmtp results
  lmtp_object <- readRDS(file.path(load_dir, paste0("lmtp_result_",outcome,".rds")))
  
  # prepare results for the observed data
  results_observed <- map_dfr(lmtp_object$observed, tidy, .id = "t") |> 
    rename(old.conf.low = conf.low,
           old.conf.high = conf.high) |>
    mutate(estimate = ifelse(t == 1, estimate, 1 - estimate),
           conf.low = ifelse(t == 1, old.conf.low, 1 - old.conf.high),
           conf.high = ifelse(t == 1, old.conf.high, 1 - old.conf.low),
           t = as.numeric(t),
           type = "observed") |>
    select(-c(old.conf.low, old.conf.high)) |>
    add_row(t = 0, estimator = "TMLE", estimate = 0, std.error = 0, conf.low = 0, conf.high = 0, type = 'observed')
  
  # prepare results for the intervention data
  results_intervention <- map_dfr(lmtp_object$intervention, tidy, .id = "t") |> 
    rename(old.conf.low = conf.low,
           old.conf.high = conf.high) |>
    mutate(estimate = ifelse(t == 1, estimate, 1 - estimate),
           conf.low = ifelse(t == 1, old.conf.low, 1 - old.conf.high),
           conf.high = ifelse(t == 1, old.conf.high, 1 - old.conf.low),
           t = as.numeric(t),
           type = "intervention") |>
    select(-c(old.conf.low, old.conf.high)) |>
    add_row(t = 0, estimator = "TMLE", estimate = 0, std.error = 0, conf.low = 0, conf.high = 0, type = 'intervention')
  
  
  # bind the observed and intervention results together
  combined_results <- rbind(results_observed,results_intervention)
}



plot_surv <- function(outcome){
  file_name <- outcome_codes |> filter(Y == outcome) |> select(name)
  title <- paste0("Estimated Incidence of ",file_name)
  
  combined_results <- combine_intervention_observed(outcome)
  
  # plot the survival curves for observed data and intervention
  p <- ggplot(combined_results, aes(x = t, y = estimate)) + 
    geom_point(aes(colour = type)) +
    geom_line(aes(colour = type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = type),alpha = 0.3, show.legend = F) +
    labs(x = "month", y = "estimated incidence", title = title) + 
    scale_x_continuous(labels = c("0","6","12","18","24")) +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_light()
  
  # save the plot
  print(p)
  ggsave(file = file.path(result_dir, paste0("surv_plot_", file_name, ".pdf")), width = 5, height = 4)
  return(p)
  
}


plot_survdiff <- function(outcome){
  # load lmtp results
  lmtp_object <- readRDS(file.path(load_dir, paste0("lmtp_result_",outcome,".rds")))
  
  file_name <- outcome_codes |> filter(Y == outcome) |> select(name)
  title <- paste0("Estimated contrasts for ",file_name)
  
  # unpack the contrast results from the lmtp_contrast object
  results_contrast <- data.frame()
  for (i in 1:4){
    results_contrast <- rbind(results_contrast, lmtp_object$contrast[[i]]$vals)
  }
  
  # prepare the contrast data for plotting
  results_contrast <- results_contrast |>
    rename(old.conf.low = conf.low,
           old.conf.high = conf.high) |>
    mutate(t = row_number(),
           theta = ifelse(t==1,theta,-theta),
           conf.low = ifelse(t==1,old.conf.low,-old.conf.high),
           conf.high = ifelse(t==1,old.conf.high,-old.conf.low)) |>
    select(-c(old.conf.low, old.conf.high)) |>
    add_row(theta = 0, shift = 0, ref = 0, std.error = 0, conf.low = 0, conf.high = 0, p.value = 0, t = 0)
  
  # make the plot
  p <- results_contrast |>
    ggplot(aes(x = t, y = theta)) +
    geom_line() +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "grey",alpha = 0.3, show.legend = F) +
    labs(
      # title = outcome,
      title = title,
      # subtitle = subtitle,
      x = "month",
      y = "difference (Y_intervention - Y_observed)"
    ) +
    theme_bw() +
    scale_x_continuous(labels = c("0","6","12","18","24")) +
    scale_y_continuous(labels = scales::label_comma())
    
  # save plot
  print(p)
  ggsave(file = file.path(result_dir, paste0("survdiff_plot_", file_name, ".pdf")), width = 4, height = 4)
  return(p)
}

plot_stepchange <- function(outcome){
  file_name <- outcome_codes |> filter(Y == outcome) |> select(name)
  title <- paste0("Estimated incidence change at \neach time step for ",file_name)
  
  combined_results <- combine_intervention_observed(outcome) |>
    group_by(type) |>
    arrange(type, t) |>
    mutate(change.estimate = estimate - lag(estimate),
           change.std.error = sqrt(std.error^2 + lag(std.error)^2),
           change.low = change.estimate - 1.96*change.std.error,
           change.high = change.estimate + 1.96*change.std.error) |>
    mutate_all(~replace_na(., 0)) |>
    ungroup() |>
    select(-c(estimate,std.error,conf.low,conf.high))
  
      # plot the survival curves for observed data and intervention
  p <- ggplot(combined_results, aes(x = t, y = change.estimate)) + 
    geom_point(aes(colour = type)) +
    geom_line(aes(colour = type)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_ribbon(aes(ymin = change.low, ymax = change.high, fill = type),alpha = 0.3, show.legend = F) +
    labs(x = "month", y = "estimate", title = title) + 
    scale_x_continuous(labels = c("0","6","12","18","24")) +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_color_manual(values = c("purple", "forestgreen")) +
    scale_fill_manual(values = c("purple", "forestgreen")) +
  
    theme_light()
  
  # save the plot
  print(p)
  ggsave(file = file.path(result_dir, paste0("change_plot_", file_name, ".pdf")), width = 5, height = 4)
  return(p)
}


  
plot_surv("Y1")
plot_survdiff("Y1")
plot_stepchange("Y1")

plot_surv("Y2")
plot_survdiff("Y2")
plot_stepchange("Y2")

plot_surv("Y3")
plot_survdiff("Y3")
plot_stepchange("Y3")

plot_surv("Y4")
plot_survdiff("Y4")
plot_stepchange("Y4")

plot_surv("Y5")
plot_survdiff("Y5")
plot_stepchange("Y5")

plot_surv("Y6")
plot_survdiff("Y6")

