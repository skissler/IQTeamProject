# ==============================================================================
# calibrate_app.R - Standalone Full-Grid Calibration for the Shiny App
# ==============================================================================
#
# Purpose:
#   Pre-computes calibrated beta values for ALL discrete parameter combinations
#   used by the app's selectInput controls. Writes app/data/calibrated_betas.csv.
#
# Grid: 4 R0 × 5 SAR_crowded × 3 fold_diff × 3 gamma = 180 rows
#
# Usage:
#   Rscript app/calibrate_app.R
#   (run from project root)
#
# ==============================================================================

library(tidyverse)
library(odin)

# ==============================================================================
# Helper Functions (copied from app/app.R and code/utils.R)
# ==============================================================================

generate_household_state_table <- function(n_min = 1, n_max = 7, crowding = FALSE) {
  states <- expand.grid(x = 0:n_max, y = 0:n_max, z = 0:n_max) %>%
    dplyr::mutate(hh_size = x + y + z) %>%
    dplyr::filter(hh_size <= n_max, hh_size >= n_min) %>%
    dplyr::arrange(hh_size, x, y, z) %>%
    dplyr::mutate(state_index = dplyr::row_number())

  find_index <- function(x_, y_, z_) {
    idx <- states %>% dplyr::filter(x == x_, y == y_, z == z_) %>% dplyr::pull(state_index)
    if (length(idx) == 0) return(0) else return(idx)
  }

  states <- states %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rec_index = if (z > 0 && y < n_max) find_index(x, y + 1, z - 1) else 0,
      inf_index = if (y > 0 && x < n_max) find_index(x + 1, y - 1, z) else 0
    ) %>%
    dplyr::ungroup()

  if (crowding) {
    n_states <- nrow(states)
    states_crowded <- states %>%
      dplyr::mutate(state_index = state_index + n_states) %>%
      dplyr::mutate(rec_index = dplyr::case_when(rec_index > 0 ~ rec_index + n_states, TRUE ~ 0)) %>%
      dplyr::mutate(inf_index = dplyr::case_when(inf_index > 0 ~ inf_index + n_states, TRUE ~ 0))
    states <- dplyr::bind_rows(
      dplyr::mutate(states, crowded = 0),
      dplyr::mutate(states_crowded, crowded = 1)
    )
  }
  return(states)
}

adjust_crowding <- function(df, fold_diff = 1, n_max = 7, indexcols = NULL) {
  out <- df %>%
    dplyr::mutate(multiplier = (1 + (fold_diff - 1) * (hhSize - 2) / (n_max - 2))) %>%
    dplyr::mutate(multiplier = dplyr::case_when(hhSize == 1 ~ 0, TRUE ~ multiplier)) %>%
    dplyr::mutate(denom = prop * multiplier) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(indexcols))) %>%
    dplyr::mutate(c = prop_crowded / sum(denom)) %>%
    dplyr::mutate(prop_crowded_adj = c * multiplier) %>%
    dplyr::select(-multiplier, -denom, -c)
  return(out)
}

make_ic_joiner <- function(dat, fold_diff = 1, n_max = 7, indexcols = NULL) {
  dat <- adjust_crowding(dat, fold_diff = fold_diff, n_max = n_max, indexcols = indexcols)
  dat_c0 <- dplyr::mutate(dat, x = hhSize, y = 0, z = 0, crowded = 0,
                           frac = prop * (1 - prop_crowded_adj))
  dat_c1 <- dplyr::mutate(dat, x = hhSize, y = 0, z = 0, crowded = 1,
                           frac = prop * prop_crowded_adj)
  out <- dplyr::bind_rows(dat_c0, dat_c1) %>%
    dplyr::select(x, y, z, hh_size = hhSize, crowded, frac)
  return(out)
}

format_output_hh <- function(model_output, household_states) {
  out <- model_output %>%
    tidyr::pivot_longer(-t, names_to = "state_index", values_to = "prop_hh") %>%
    dplyr::mutate(subpop = substr(state_index, 3, 3)) %>%
    dplyr::mutate(state_index = as.numeric(substr(state_index, 5, nchar(state_index) - 1))) %>%
    dplyr::left_join(
      dplyr::select(household_states, x, y, z, hh_size, state_index, crowded),
      by = "state_index"
    )
  return(out)
}

format_output_indiv <- function(model_output, household_states) {
  out_hh <- format_output_hh(model_output, household_states)
  out <- out_hh %>%
    dplyr::mutate(S_num = prop_hh * x, I_num = prop_hh * y, R_num = prop_hh * z, den = prop_hh * hh_size) %>%
    dplyr::group_by(t, subpop) %>%
    dplyr::summarise(S_num = sum(S_num), I_num = sum(I_num), R_num = sum(R_num), den = sum(den), .groups = "drop") %>%
    dplyr::mutate(S_indiv = S_num / den, I_indiv = I_num / den, R_indiv = R_num / den) %>%
    dplyr::select(t, subpop, S_indiv, I_indiv, R_indiv)
  return(out)
}

calculate_tau <- function(sar, gamma) {
  sar * gamma / (1 - sar)
}

calculate_tau_boost <- function(sar_crowded, gamma, tau) {
  tau_crowded <- calculate_tau(sar_crowded, gamma)
  tau_crowded - tau
}

# ==============================================================================
# Define Odin Model (same as in app.R)
# ==============================================================================

household_model_twopop_crowding <- odin::odin({
  n_states <- user()
  x[] <- user()
  y[] <- user()
  z[] <- user()
  hh_size[] <- user()
  crowded[] <- user()
  rec_index[] <- user()
  inf_index[] <- user()
  init_C[] <- user()
  init_A[] <- user()

  gamma <- user()
  tau <- user()
  tau_boost <- user()
  beta <- user()
  eps <- user()
  pop_C <- user()
  pop_A <- user()

  dim(x) <- n_states
  dim(y) <- n_states
  dim(z) <- n_states
  dim(hh_size) <- n_states
  dim(crowded) <- n_states
  dim(rec_index) <- n_states
  dim(inf_index) <- n_states
  dim(init_C) <- n_states
  dim(init_A) <- n_states
  dim(H_C) <- n_states
  dim(H_A) <- n_states

  initial(H_C[]) <- init_C[i]
  initial(H_A[]) <- init_A[i]

  dim(I_num_C) <- n_states
  dim(I_den_C) <- n_states
  dim(I_num_A) <- n_states
  dim(I_den_A) <- n_states

  I_num_C[] <- H_C[i] * y[i]
  I_den_C[] <- H_C[i] * hh_size[i]
  I_C <- sum(I_num_C) / sum(I_den_C)

  I_num_A[] <- H_A[i] * y[i]
  I_den_A[] <- H_A[i] * hh_size[i]
  I_A <- sum(I_num_A) / sum(I_den_A)

  w_C <- pop_C / (pop_C + pop_A)
  w_A <- pop_A / (pop_C + pop_A)

  m_CC <- (1 - eps) + eps * w_C
  m_CA <- eps * w_A
  m_AC <- eps * w_C
  m_AA <- (1 - eps) + eps * w_A

  lambda_C <- beta * (m_CC * I_C + m_CA * I_A)
  lambda_A <- beta * (m_AC * I_C + m_AA * I_A)

  deriv(H_C[]) <-
    gamma * (-y[i] * H_C[i] + if (rec_index[i] > 0) (y[i] + 1) * H_C[rec_index[i]] else 0) +
    (tau + tau_boost*crowded[i]) * (-x[i] * y[i] * H_C[i] + if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_C[inf_index[i]] else 0) +
    lambda_C * (-x[i] * H_C[i] + if (inf_index[i] > 0) (x[i] + 1) * H_C[inf_index[i]] else 0)

  deriv(H_A[]) <-
    gamma * (-y[i] * H_A[i] + if (rec_index[i] > 0) (y[i] + 1) * H_A[rec_index[i]] else 0) +
    (tau + tau_boost*crowded[i]) * (-x[i] * y[i] * H_A[i] + if (inf_index[i] > 0) (x[i] + 1) * (y[i] - 1) * H_A[inf_index[i]] else 0) +
    lambda_A * (-x[i] * H_A[i] + if (inf_index[i] > 0) (x[i] + 1) * H_A[inf_index[i]] else 0)
})

# ==============================================================================
# Aggregate ACS Data to National Level
# ==============================================================================

acs_regional <- read_csv("app/data/acs_data_regional.csv", show_col_types = FALSE)

nat_data <- acs_regional %>%
  mutate(
    prop = prop * population,
    prop_crowded = prop_crowded * population
  ) %>%
  group_by(hhSize) %>%
  summarise(
    prop = sum(prop),
    prop_crowded = sum(prop_crowded),
    population = sum(population),
    .groups = "drop"
  ) %>%
  mutate(
    prop = prop / population,
    prop_crowded = prop_crowded / population
  )

# ==============================================================================
# Calibration Functions (from code/calibrate_model.R)
# ==============================================================================

solve_final_size <- function(r0, tol = 1e-10, max_iter = 1000) {
  if (r0 <= 1) return(0)
  r_inf <- 0.5
  for (i in 1:max_iter) {
    r_inf_new <- 1 - exp(-r0 * r_inf)
    if (abs(r_inf_new - r_inf) < tol) return(r_inf_new)
    r_inf <- r_inf_new
  }
  warning("solve_final_size did not converge")
  return(r_inf)
}

get_final_attack_rate <- function(beta, base_pars, nat_data) {
  pars <- base_pars
  pars$beta <- beta
  result <- run_calibration_sim(pars, nat_data)
  result_C <- result %>% ungroup() %>% filter(subpop == "C")
  final_R <- result_C$R_indiv[which.max(result_C$t)]
  return(final_R)
}

calibrate_beta <- function(target_r0, base_pars, nat_data,
                           tol = 0.0005, max_iter = 50,
                           beta_lower = 0.01 * base_pars$gamma,
                           beta_upper = 10.0 * base_pars$gamma) {
  target_final_size <- solve_final_size(target_r0)

  cat("Calibrating for R0 =", target_r0, "\n")
  cat("  Target final size:", round(target_final_size, 4), "\n")

  for (iter in 1:max_iter) {
    beta_mid <- (beta_lower + beta_upper) / 2
    sim_final_size <- get_final_attack_rate(beta_mid, base_pars, nat_data)
    error <- sim_final_size - target_final_size

    cat("  Iter", iter, ": beta =", round(beta_mid, 6),
        ", final_size =", round(sim_final_size, 4),
        ", error =", round(error, 5), "\n")

    if (abs(error) < tol) {
      cat("  Converged! beta =", round(beta_mid, 6), "\n\n")
      return(list(
        r0 = target_r0,
        beta = beta_mid,
        target_final_size = target_final_size,
        simulated_final_size = sim_final_size,
        iterations = iter
      ))
    }

    if (error > 0) {
      beta_upper <- beta_mid
    } else {
      beta_lower <- beta_mid
    }
  }

  warning("Calibration did not converge for R0 = ", target_r0)
  return(list(r0 = target_r0, beta = beta_mid,
              target_final_size = target_final_size,
              simulated_final_size = sim_final_size,
              iterations = max_iter))
}

# Pre-generate household state table
household_states <- generate_household_state_table(n_min = 1, n_max = 7, crowding = TRUE)

run_calibration_sim <- function(pars, nat_data) {
  with(as.list(pars), {
    household_states_local <- generate_household_state_table(
      n_min = 1, n_max = max_hh_size, crowding = TRUE
    )
    n_states <- nrow(household_states_local)

    ic_joiner <- make_ic_joiner(nat_data, fold_diff = crowding_fold_diff)

    ic_joiner_inf <- ic_joiner %>%
      mutate(frac = init_prev * frac * hh_size) %>%
      mutate(y = y + 1, x = x - 1)
    ic_joiner$frac <- ic_joiner$frac - ic_joiner_inf$frac
    ic_joiner <- bind_rows(ic_joiner, ic_joiner_inf)

    init_nat_C <- household_states_local %>%
      left_join(ic_joiner, by = c("x", "y", "z", "hh_size", "crowded")) %>%
      arrange(state_index) %>%
      replace_na(list(frac = 0)) %>%
      pull(frac)

    init_nat_A <- rep(0, n_states)
    init_nat_A[1] <- 1

    mod_national <- household_model_twopop_crowding$new(
      n_states = n_states,
      x = household_states_local$x,
      y = household_states_local$y,
      z = household_states_local$z,
      hh_size = household_states_local$hh_size,
      crowded = household_states_local$crowded,
      rec_index = household_states_local$rec_index,
      inf_index = household_states_local$inf_index,
      init_C = init_nat_C,
      init_A = init_nat_A,
      gamma = gamma,
      tau = tau,
      tau_boost = tau_boost,
      beta = beta,
      eps = eps,
      pop_C = 10000,
      pop_A = 0
    )

    times <- seq(0, 1000, by = 1)
    out_national <- as_tibble(data.frame(mod_national$run(times)))
    epidf_indiv_national <- format_output_indiv(out_national, household_states_local)
    return(epidf_indiv_national)
  })
}

# ==============================================================================
# Full-Grid Calibration
# ==============================================================================

sar_uncrowded <- 0.20
r0_values        <- c(1.2, 1.5, 2.0, 3.0)
sar_crowded_vals <- c(0.20, 0.30, 0.40, 0.50, 0.60)
fold_diff_vals   <- c(1, 2, 3)
gamma_vals       <- c(1/3, 1/5, 1/10)

# Full cross: 4 × 5 × 3 × 3 = 180 rows
calibration_grid <- expand.grid(
  r0 = r0_values,
  sar_crowded = sar_crowded_vals,
  fold_diff = fold_diff_vals,
  gamma = gamma_vals,
  stringsAsFactors = FALSE
) %>%
  arrange(gamma, sar_crowded, fold_diff, r0)

cat("\n========================================\n")
cat("Full-Grid Calibration for App\n")
cat("========================================\n")
cat("R0 values:", paste(r0_values, collapse = ", "), "\n")
cat("SAR crowded values:", paste(sar_crowded_vals, collapse = ", "), "\n")
cat("Fold diff values:", paste(fold_diff_vals, collapse = ", "), "\n")
cat("Gamma values:", paste(round(gamma_vals, 4), collapse = ", "), "\n")
cat("Total calibrations:", nrow(calibration_grid), "\n")
cat("========================================\n\n")

calibration_results <- vector("list", nrow(calibration_grid))

for (i in seq_len(nrow(calibration_grid))) {
  row <- calibration_grid[i, ]

  # Build parameter set for this grid row
  cal_pars <- list(
    gamma = row$gamma,
    sar_uncrowded = sar_uncrowded,
    sar_crowded = row$sar_crowded,
    eps = 0,
    max_hh_size = 7,
    crowding_fold_diff = row$fold_diff,
    init_prev = 0.001
  )
  cal_pars$tau <- calculate_tau(cal_pars$sar_uncrowded, cal_pars$gamma)
  cal_pars$tau_boost <- calculate_tau_boost(row$sar_crowded, cal_pars$gamma, cal_pars$tau)

  cat(sprintf("\n--- Grid row %d/%d: R0=%.1f, SAR_crowded=%.2f, fold_diff=%d, gamma=%.4f ---\n",
              i, nrow(calibration_grid), row$r0, row$sar_crowded, row$fold_diff, row$gamma))

  result <- calibrate_beta(row$r0, cal_pars, nat_data, tol = 0.0005)
  result$sar_crowded <- row$sar_crowded
  result$fold_diff <- row$fold_diff
  result$gamma <- row$gamma
  calibration_results[[i]] <- result
}

# Build output data frame
calibrated_betas_df <- tibble(
  r0 = sapply(calibration_results, `[[`, "r0"),
  sar_crowded = sapply(calibration_results, `[[`, "sar_crowded"),
  fold_diff = sapply(calibration_results, `[[`, "fold_diff"),
  gamma = sapply(calibration_results, `[[`, "gamma"),
  beta = sapply(calibration_results, `[[`, "beta"),
  target_final_size = sapply(calibration_results, `[[`, "target_final_size"),
  simulated_final_size = sapply(calibration_results, `[[`, "simulated_final_size"),
  iterations = sapply(calibration_results, `[[`, "iterations")
)

write_csv(calibrated_betas_df, "app/data/calibrated_betas.csv")

cat("\n========================================\n")
cat("Calibration Complete\n")
cat("========================================\n")
cat("Total rows:", nrow(calibrated_betas_df), "\n")
cat("Saved to: app/data/calibrated_betas.csv\n")
cat("========================================\n")
