pars_list <- list(
  list(
    gamma = 1/5,
    tau_C = (1/4)*(1/5),             
    tau_A = (1/4)*(1/5),
    tau_boost = (2/3)*(1/5) - (1/4)*(1/5),  
    beta_C = .765*(1/5),      # Calibrated beta scalar
    beta_A = .765*(1/5),      # Calibrated beta scalar
    eps = 0.33,
    max_hh_size = 7,
    crowding_fold_diff=2,
    adjust_hhvars = TRUE,
    init_prev = 0.001
  )
)