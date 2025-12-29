pars_list <- list(
  list(                      # 1: DEFAULT
    parset = 1,              # Parameter set 
    gamma = 1/5,             # Infectious period: 5 days
    tau_C = (1/4)*(1/5),     # Baseline community SAR: 20%
    tau_A = (1/4)*(1/5),     # Baseline agricultural SAR: 20%
    tau_boost = (2/3)*(1/5) - (1/4)*(1/5),  # SAR boost: to 40%
    beta_C = .765*(1/5),     # Community R0: 1.2
    beta_A = .765*(1/5),     # Agricultural R0: 1.2
    eps = 0.33,              # Assortment: 
    max_hh_size = 7,         # Max household size: 
    crowding_fold_diff=2,    # Crowding frequency for max-size vs. size-2 hhs: 2
    adjust_hhvars = TRUE,    # Adjust county-level NAWS hh variables: TRUE
    init_prev = 0.001        # Initial prevalence: 0.001
  ),
  list(                      # 2: SENSITIVITY 1: R0 = 1.5
    parset = 2,              # Parameter set 
    gamma = 1/5,             # Infectious period: 5 days
    tau_C = (1/4)*(1/5),     # Baseline community SAR: 20%
    tau_A = (1/4)*(1/5),     # Baseline agricultural SAR: 20%
    tau_boost = (2/3)*(1/5) - (1/4)*(1/5),  # SAR boost: to 40%
    beta_C = 1.05*(1/5),     # Community R0: 1.5
    beta_A = 1.05*(1/5),     # Agricultural R0: 1.5
    eps = 0.33,              # Assortment: 
    max_hh_size = 7,         # Max household size: 
    crowding_fold_diff=2,    # Crowding frequency for max-size vs. size-2 hhs: 2
    adjust_hhvars = TRUE,    # Adjust county-level NAWS hh variables: TRUE
    init_prev = 0.001        # Initial prevalence: 0.001
  ),
  list(                      # 2: SENSITIVITY 2: R0 = 2
    parset = 3,              # Parameter set 
    gamma = 1/5,             # Infectious period: 5 days
    tau_C = (1/4)*(1/5),     # Baseline community SAR: 20%
    tau_A = (1/4)*(1/5),     # Baseline agricultural SAR: 20%
    tau_boost = (2/3)*(1/5) - (1/4)*(1/5),  # SAR boost: to 40%
    beta_C = 1.53*(1/5),     # Community R0: 2.0
    beta_A = 1.53*(1/5),     # Agricultural R0: 2.0
    eps = 0.33,              # Assortment: 
    max_hh_size = 7,         # Max household size: 
    crowding_fold_diff=2,    # Crowding frequency for max-size vs. size-2 hhs: 2
    adjust_hhvars = TRUE,    # Adjust county-level NAWS hh variables: TRUE
    init_prev = 0.001        # Initial prevalence: 0.001
  ),
  list(                      # 2: SENSITIVITY 3: R0 = 3
    parset = 4,              # Parameter set 
    gamma = 1/5,             # Infectious period: 5 days
    tau_C = (1/4)*(1/5),     # Baseline community SAR: 20%
    tau_A = (1/4)*(1/5),     # Baseline agricultural SAR: 20%
    tau_boost = (2/3)*(1/5) - (1/4)*(1/5),  # SAR boost: to 40%
    beta_C = 2.52*(1/5),     # Community R0: 3.0
    beta_A = 2.52*(1/5),     # Agricultural R0: 3.0
    eps = 0.33,              # Assortment: 
    max_hh_size = 7,         # Max household size: 
    crowding_fold_diff=2,    # Crowding frequency for max-size vs. size-2 hhs: 2
    adjust_hhvars = TRUE,    # Adjust county-level NAWS hh variables: TRUE
    init_prev = 0.001        # Initial prevalence: 0.001
  )
)