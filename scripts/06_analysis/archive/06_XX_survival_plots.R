# -------------------------------------
# Script: survival_plots
# Author:
# Purpose:
# Notes: THIS IS AN OLD VERSION.
# -------------------------------------
library(tidyverse)
library(purrr)
library(lmtp)
library(gridExtra)
library(grid)
library(gtable)

load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/analysis/ver3"
result_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/analysis/ver3/plots"

outcome_codes <- data.frame(Y = c("Y1", "Y2", "Y3", "Y4"),
                            name = c("Overdose","hillary","OUD_all_inclusive","MOUD"))

combine_intervention_observed <- function(outcome, shift, c_section_identifier) {
  # load lmtp results
  lmtp_object <- readRDS(file.path(load_dir, paste0("lmtp_result_", c_section_identifier, "_", shift, "_", outcome,".rds")))
  
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



plot_surv <- function(outcome, shift, c_section_identifier, title){
  # file_name <- outcome_codes |> filter(Y == outcome) |> select(name)
  # title <- paste0("Estimated Incidence of ",file_name, ",\n", shift, ", ", c_section_identifier)
  
  combined_results <- combine_intervention_observed(outcome, shift, c_section_identifier)
  
  # plot the survival curves for observed data and intervention
  p <- ggplot(combined_results, aes(x = t, y = estimate)) + 
    geom_point(aes(colour = type), size = 3) +
    geom_line(aes(colour = type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = type),alpha = 0.3, show.legend = F) +
    labs(x = "month", y = "estimated incidence", title = title) + 
    scale_x_continuous(labels = c("0","6","12","18","24")) +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0,0.025)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank())
  
  # save the plot
  # print(p)
  # ggsave(file = file.path(result_dir, paste0("surv_plot_", c_section_identifier, "_", shift, "_", file_name, ".pdf")), width = 5, height = 4)
  return(p)
  
}


plot_survdiff <- function(outcome, shift, c_section_identifier, title){
  # load lmtp results
  lmtp_object <- readRDS(file.path(load_dir, paste0("lmtp_result_", c_section_identifier, "_", shift, "_", outcome,".rds")))
  
  # file_name <- outcome_codes |> filter(Y == outcome) |> select(name)
  # title <- paste0("Estimated contrasts for ",file_name, ",\n", shift, ", ", c_section_identifier)
  
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
  p <- ggplot(results_contrast, aes(x = t, y = theta)) +
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
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
    scale_x_continuous(labels = c("0","6","12","18","24")) +
    scale_y_continuous(labels = scales::label_comma())
    
  # save plot
  # print(p)
  # ggsave(file = file.path(result_dir, paste0("survdiff_plot_", c_section_identifier, "_", shift, "_", file_name, ".pdf")), width = 4, height = 4)
  return(p)
}

# plot_stepchange <- function(outcome){
#   # file_name <- outcome_codes |> filter(Y == outcome) |> select(name)
#   # title <- paste0("Estimated incidence change at \neach time step for ",file_name)
#   
#   combined_results <- combine_intervention_observed(outcome) |>
#     group_by(type) |>
#     arrange(type, t) |>
#     mutate(change.estimate = estimate - lag(estimate),
#            change.std.error = sqrt(std.error^2 + lag(std.error)^2),
#            change.low = change.estimate - 1.96*change.std.error,
#            change.high = change.estimate + 1.96*change.std.error) |>
#     mutate_all(~replace_na(., 0)) |>
#     ungroup() |>
#     select(-c(estimate,std.error,conf.low,conf.high))
#   
#       # plot the survival curves for observed data and intervention
#   p <- ggplot(combined_results, aes(x = t, y = change.estimate)) + 
#     geom_point(aes(colour = type)) +
#     geom_line(aes(colour = type)) +
#     geom_hline(yintercept = 0, linetype = "dotted") +
#     geom_ribbon(aes(ymin = change.low, ymax = change.high, fill = type),alpha = 0.3, show.legend = F) +
#     labs(x = "month", y = "estimate", title = title) + 
#     scale_x_continuous(labels = c("0","6","12","18","24")) +
#     scale_y_continuous(labels = scales::label_comma()) +
#     scale_color_manual(values = c("purple", "forestgreen")) +
#     scale_fill_manual(values = c("purple", "forestgreen")) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#   
#     theme_light()
#   
#   # save the plot
#   # print(p)
#   # ggsave(file = file.path(result_dir, paste0("change_plot_", file_name, ".pdf")), width = 5, height = 4)
#   return(p)
# }





plots_surv <- list()
plots_diff <- list()
labels <- c("Shift1","Shift2","Shift3")
i <- 1

for (Y in c("Y2")){
  for (shift in c("shift_1", "shift_2", "shift_3")){
    plots_surv[[i]] <- plot_surv(Y, shift, "other", labels[i])
    plots_diff[[i]] <- plot_survdiff(Y, shift, "other", labels[i])
    i <- i + 1
  }
}



# Arrange the plots and the legend
grid.arrange(grobs = plots_surv, 
             nrow = 1, 
             top = "Title", 
             left = "Y-label",
             right = legend,
             padding = unit(1, "cm"))

grid.text("Overall X Label", x = 0.5, y = 0.05, gp = gpar(fontsize = 14))
grid.text("Overall Y Label", x = 0.05, y = 0.5, rot = 90, gp = gpar(fontsize = 14))

# Create a separate legend
legend <- gtable_filter(ggplot_gtable(ggplot_build(plots_surv[[1]])), "guide-box") 

# things to adjust
- y axis limits
- titles: make into just letters?
- legend: remove all and make just one on the side



for (shift in c("shift_1", "shift_2", "shift_3")){
  plot_surv("Y2", shift, "c-section")
  plot_survdiff("Y2", shift, "c-section")
}

for (shift in c("shift_1", "shift_2", "shift_3")){
  plot_surv("Y3", shift, "c-section")
  plot_survdiff("Y3", shift, "c-section")
}

for (shift in c("shift_1", "shift_2", "shift_3")){
  plot_surv("Y4", shift, "c-section")
  plot_survdiff("Y4", shift, "c-section")
}


