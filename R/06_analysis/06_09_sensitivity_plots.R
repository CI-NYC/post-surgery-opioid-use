# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(tidyverse)
library(lmtp)
library(gridExtra)

load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/analysis"
result_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/sensitivity_analysis/analysis/plots"



combine_intervention_observed <- function(outcome, shift, c_section_identifier) {
  '
  This function loads in the results made from LMTP.
  It combines the intervention estimates with the observed estimates into a 
  single dataframe for ease of use with ggplot.
  Some additional rows of 0s are added so that the plots will display 0 incidence at time 0
  '
  
  # load lmtp results
  lmtp_object <- readRDS(file.path(load_dir, paste0("lmtp_result_", c_section_identifier, "_", shift, "_", outcome,".rds")))
  
  # prepare results for the observed data
  results_observed <- map_dfr(lmtp_object$observed, tidy, .id = "t") |> 
    rename(old.conf.low = conf.low,
           old.conf.high = conf.high) |>
    mutate(estimate = 1-estimate,
           conf.low = 1-old.conf.high,
           conf.high = 1-old.conf.low,
           t = as.numeric(t),
           type = "no reduction") |>
    select(-c(old.conf.low, old.conf.high)) |>
    add_row(t = 0, estimator = "TMLE", estimate = 0, std.error = 0, conf.low = 0, conf.high = 0, type = 'no reduction')
  
  # prepare results for the intervention data
  results_intervention <- map_dfr(lmtp_object$intervention, tidy, .id = "t") |> 
    rename(old.conf.low = conf.low,
           old.conf.high = conf.high) |>
    mutate(estimate = 1-estimate,
           conf.low = 1-old.conf.high,
           conf.high = 1-old.conf.low,
           t = as.numeric(t),
           type = "hypothetical reduction") |>
    select(-c(old.conf.low, old.conf.high)) |>
    add_row(t = 0, estimator = "TMLE", estimate = 0, std.error = 0, conf.low = 0, conf.high = 0, type = 'hypothetical reduction')
  
  
  # bind the observed and intervention results together
  combined_results <- rbind(results_observed,results_intervention) |>
    rename(Intervention = type)
}


plot_surv <- function(outcome, c_section_identifier){
  '
  This function plots survival plots of our three interventions side-by-side
  '
  
  combined_results <- rbind(combine_intervention_observed(outcome, "shift_1", c_section_identifier) |>
                              mutate(shift = "i"),
                            combine_intervention_observed(outcome, "shift_2", c_section_identifier) |>
                              mutate(shift = "ii"),
                            combine_intervention_observed(outcome, "shift_3", c_section_identifier) |>
                              mutate(shift = "iii"))
  
  write.csv(combined_results, paste0(result_dir, "/lmtp_results_", c_section_identifier, "_", outcome, ".csv"))
  
  # plot the survival curves for observed data and intervention
  p <- ggplot(combined_results, aes(x = t, y = estimate)) + 
    geom_point(aes(colour = Intervention), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = Intervention), width = 0.3) + 
    
    # geom_line(aes(colour = Intervention)) +
    # geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Intervention), alpha = 0.3, show.legend = F) +
    labs(x = "", y = "Absolute risk", title="(A)") + 
    scale_x_continuous(labels = c("0","6","12","18","24")) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.0001)) +
    facet_grid(cols = vars(shift)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(color = "black"),
          plot.margin = unit(c(5.5, 5.5, 5.5, 9.5), "pt"),
          strip.text.x = element_text(size = 14))
  
  # save the plot
  print(p)
  # ggsave(file = file.path(result_dir, paste0("surv_plot_", c_section_identifier, "_", title, ".pdf")), width = 15, height = 4)
  return(p)
  
}

combine_contrasts <- function(outcome, shift, c_section_identifier){
  '
  This function converts the LMTP contrasts object into a dataframe
  '
  
  lmtp_object <- readRDS(file.path(load_dir, paste0("lmtp_result_", c_section_identifier, "_", shift, "_", outcome,".rds")))
  
  results_contrast <- data.frame()
  for (i in 1:4){
    results_contrast <- rbind(results_contrast, lmtp_object$contrast[[i]]$vals)
  }
  # prepare the contrast data for plotting
  results_contrast <- results_contrast |>
    rename(old.conf.low = conf.low,
           old.conf.high = conf.high) |>
    mutate(t = row_number(),
           theta = -theta,
           conf.low = -old.conf.high,
           conf.high = -old.conf.low) |>
    select(-c(old.conf.low, old.conf.high)) |>
    add_row(theta = 0, shift = 0, ref = 0, std.error = 0, conf.low = 0, conf.high = 0, p.value = 0, t = 0)
  
  return(results_contrast)
}

plot_survdiff <- function(outcome, c_section_identifier){
  '
  This function uses the LMTP contrast results to make plots displaying the 
  difference between the intervention estimates and the observed estimates
  over the course of the study.
  It produces 3 plots side-by-side for each of our shifts.
  
  One extra consideration is that we wanted it to align perfectly with the survival plots.
  However, this contrast plot does not need a legend, and therefore, it would appear
  too wide when combining it with the survival plots using grid.arrange()
  
  The solution was to create a dummy legend, and to make the legend white so that 
  it is not actually visible. Its job is to take up the exact same amount of horizontal
  space as the legend in the survival plot, so that these two plots can have the same
  dimensions.
  
  To make ggplot auto-generate a legend, a white, unseen line is drawn at y = 0. 
  This makes its key an invisible white line on a white background
  '
  
  # load lmtp results
  results_contrast <- rbind(combine_contrasts(outcome, "shift_1", c_section_identifier) |>
                              mutate(shift = "i"),
                            combine_contrasts(outcome, "shift_2", c_section_identifier) |>
                              mutate(shift = "ii"),
                            combine_contrasts(outcome, "shift_3", c_section_identifier) |>
                              mutate(shift = "iii")
  ) |>
    mutate(Intervention="hypothetical reduction") # This column has no useful meaning whatsoever. It is used in the fake legend because the longest word in the survival plot legend is "hypothetical reduction". Therefore, this will make sure that both legends are the same width.
  
  write.csv(results_contrast, paste0(result_dir, "/lmtp_contrasts_", c_section_identifier, "_", outcome, ".csv"))
  
  # make the plot
  p <- ggplot(results_contrast, aes(x = t, y = theta)) +
    geom_line(aes(x=t, y=c(rep(0,length(theta))), color = Intervention)) + # making a white, unnoticeable line at y=0 for legend hiding purposes
    # geom_line() +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) + 
    
    geom_hline(yintercept = 0, linetype = "dotted") +
    # geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "grey",alpha = 0.3, show.legend = F) +
    labs(
      x = "month",
      y = expression(paste("Risk difference")),
      title = "(B)"
    ) +
    facet_grid(cols = vars(shift)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_text(color = "white"), # hide the legend title
          legend.text = element_text(color = "white"), # hide the legend labels
          strip.text = element_text(color = "black"),
          strip.text.x = element_text(size = 14)) +
    scale_color_manual(values = c("white")) + # making the line defined in the first line of this plot white and hidden. 
    scale_x_continuous(labels = c("0","6","12","18","24")) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.0001))
  
  # save plot
  print(p)
  # ggsave(file = file.path(result_dir, paste0("survdiff_plot_", c_section_identifier, "_", title ".pdf")), width = 12, height = 4)
  return(p)
}

p1 <- plot_surv("Y2", "other")
p2 <- plot_survdiff("Y2", "other")
pdf(file.path(result_dir, "plots_other_hillary.pdf"), width = 9, height = 6)
grid.arrange(p1, p2, ncol= 1)
dev.off()

p1 <- plot_surv("Y3", "other")
p2 <- plot_survdiff("Y3", "other")
pdf(file.path(result_dir, "plots_other_OUD.pdf"), width = 9, height = 6)
grid.arrange(p1, p2, ncol= 1)
dev.off()

p1 <- plot_surv("Y4", "other")
p2 <- plot_survdiff("Y4", "other")
pdf(file.path(result_dir, "plots_other_MOUD.pdf"), width = 9, height = 6)
grid.arrange(p1, p2, ncol = 1)
dev.off()




p1 <- plot_surv("Y2", "c-section")
p2 <- plot_survdiff("Y2", "c-section")
pdf(file.path(result_dir, "plots_c-section_hillary.pdf"), width = 9, height = 6)
grid.arrange(p1, p2, ncol= 1)
dev.off()

p1 <- plot_surv("Y3", "c-section")
p2 <- plot_survdiff("Y3", "c-section")
pdf(file.path(result_dir, "plots_c-section_OUD.pdf"), width = 9, height = 6)
grid.arrange(p1, p2, ncol= 1)
dev.off()

p1 <- plot_surv("Y4", "c-section")
p2 <- plot_survdiff("Y4", "c-section")
pdf(file.path(result_dir, "plots_c-section_MOUD.pdf"), width = 9, height = 6)
grid.arrange(p1, p2, ncol= 1)
dev.off()
