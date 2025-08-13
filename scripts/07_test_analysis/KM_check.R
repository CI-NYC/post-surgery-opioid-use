library(survival)

set.seed(123)

# Number of individuals
n <- 100

# random start dates
start_date <- as.Date('2020-01-01') + sample(0:40, n, replace = TRUE)

# random survival times (in days)
survival_time <- runif(n, min = 0, max = 40)

# random censoring times (in days)
censoring_time <- runif(n, min = 0, max = 40)

# time and status
observed_time <- pmin(survival_time, censoring_time)
status <- ifelse(survival_time <= censoring_time, 1, 0)

# Continuous data frame
simulated_data <- data.frame(
  id = 1:n,
  start_date = start_date,
  observed_time = observed_time,
  status = status
)

# Create the survival object
surv_obj <- with(simulated_data, Surv(observed_time, status))

# Fit the survival model
continuous_fit <- survfit(surv_obj ~ 1)


# Discrete data frame, prioritizing outcome if it occurs before censoring
outcome_data <- simulated_data |>
  mutate(observed_time = ceiling(observed_time / 10))

surv_obj <- with(outcome_data, Surv(observed_time, status))

# Fit the survival model
outcome_fit <- survfit(surv_obj ~ 1)



continuous_data <- data.frame(
  time = continuous_fit$time,
  surv = 1-continuous_fit$surv
)

outcome_first_data <- data.frame(
  time = c(0,outcome_fit$time*10),
  surv = c(0,1-outcome_fit$surv)
)

# combine the data frames
combined_data <- rbind(
  data.frame(time = outcome_first_data$time, surv = outcome_first_data$surv, group = "Discretized - prioritize outcome"),
  data.frame(time = continuous_data$time, surv = continuous_data$surv, group = "Continuous")
)

# plot curves
ggplot(combined_data, aes(x = time, y = surv, color = group)) +
  geom_step() +
  labs(x = "Time", y = "Cumulative Incidence", title = "") +
  theme_minimal()

