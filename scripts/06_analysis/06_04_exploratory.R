# -------------------------------------
# Script: exploratory
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(corrplot)
library(ggplot2)
library(data.table)
library(gridExtra)

df <- rbind(readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/df_non_c_section.rds"),
            readRDS("/mnt/general-data/disability/post_surgery_opioid_use/final/df_only_c_section.rds")) |>
  as.data.table()

corr_matrix <- cor(cbind(df$days_of_continuous_use, df$days_supplied))
corrplot(corr_matrix, method = "circle")

# regular scatter plot with line of best fit
ggplot(df, aes(x=days_of_continuous_use, y=days_supplied)) +
  # geom_point() +
  geom_jitter() +
  geom_smooth() +
  facet_grid(. ~ Y2_4) +
# labs(x="Miles Per Gallon", y="Horsepower", title="Scatter plot of MPG vs Horsepower") +
  theme_minimal()

boxplot_df <- rbind(df |> select(var = days_of_continuous_use, Y2_4, Y3_4, Y4_4) |> mutate(exposure = "cont_use"),
                    df |> select(var = days_supplied, Y2_4, Y3_4, Y4_4) |> mutate(exposure = "days_supplied"))
  
ggplot(boxplot_df, aes(x=exposure, y=var, fill = as.factor(Y2_4))) +
  geom_boxplot() +
  # geom_jitter() +
  ylim(0,20) + 
  geom_violin()


p1 <- ggplot(df[Y3_4 == 0], aes(x=days_supplied)) +
  geom_histogram() +
  xlim(0,50) +
  theme_minimal()
p2 <- ggplot(df[Y3_4 == 1], aes(x=days_supplied)) +
  geom_histogram() +
  xlim(0,50) +
  theme_minimal()
p3 <- ggplot(df[Y3_4 == 0], aes(x=days_of_continuous_use)) +
  geom_histogram() +
  xlim(0,50) +
  theme_minimal()
p4 <- ggplot(df[Y3_4 == 1], aes(x=days_of_continuous_use)) +
  geom_histogram() +
  xlim(0,50) +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol=2)


# # MA plot
# average <- rowMeans(exposures[,c("days_of_continuous_use", "days_supplied")])
# ratio <- exposures$days_of_continuous_use / exposures$days_supplied
# 
# # Create a new dataframe for the plot
# plot_df <- data.frame(average=average, ratio=ratio)
# 
# # Create the MA plot
# ggplot(plot_df, aes(x=average, y=ratio)) +
#   # geom_point() +
#   geom_jitter() +
#   theme_minimal()
