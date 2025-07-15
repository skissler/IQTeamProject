joint_data <- readRDS("data/processed/joint_data.rds")
fit_biv <- readRDS("results/fit_bivariate.rds")

theta_draws <- rstan::extract(fit_biv, pars = "theta_pred")$theta_pred

joint_data$ag_crowded_median <- apply(theta_draws[, , 1], 2, median)
joint_data$ag_crowded_low    <- apply(theta_draws[, , 1], 2, quantile, probs = 0.1)
joint_data$ag_crowded_high   <- apply(theta_draws[, , 1], 2, quantile, probs = 0.9)

joint_data$ag_kids_median <- apply(theta_draws[, , 2], 2, median)
joint_data$ag_kids_low    <- apply(theta_draws[, , 2], 2, quantile, probs = 0.1)
joint_data$ag_kids_high   <- apply(theta_draws[, , 2], 2, quantile, probs = 0.9)

readr::write_csv(joint_data, "results/posterior_summaries.csv")