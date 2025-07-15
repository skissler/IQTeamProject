library(odin)

generate_household_states <- function(n_max) {
  states <- data.frame()
  idx <- 1
  for (n in 1:n_max) {
    for (x in 0:n) {
      for (y in 0:(n - x)) {
        z <- n - x - y
        states <- rbind(states, data.frame(
          index = idx, x = x, y = y, z = z, n = n
        ))
        idx <- idx + 1
      }
    }
  }
  return(states)
}

household_states <- generate_household_states(8)


map_household_states <- function(household_states){

# Helper function to get state index
  find_index <- function(states, x_, y_, z_) {
    idx <- states %>%
      dplyr::filter(x == x_, y == y_, z == z_) %>%
      dplyr::pull(household_states_index)
    if (length(idx) == 0) return(0) else return(idx)
  }

  # Compute transitions
  household_states <- household_states %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      recover_to = if (y > 0) find_index(household_states, x, y - 1, z + 1) else 0,
      within_to  = if (x > 0) find_index(household_states, x - 1, y + 1, z) else 0,
      between_to = within_to
    ) %>%
    dplyr::ungroup()

  return(household_states)

}



household_model <- odin::odin({
  # Parameters
  n_states <- user()
  x[] <- user()
  y[] <- user()
  z[] <- user()
  hh_size[] <- user()

  gamma <- user()
  tau <- user()
  beta <- user()

  dim(H) <- n_states
  dim(dH) <- n_states

  # Total prevalence across all households
  dim(S_num) <- n_states
  dim(I_num) <- n_states
  dim(R_num) <- n_states
  dim(den) <- n_states

  S_num <- H[i] * x[i]
  I_num <- H[i] * y[i]
  R_num <- H[i] * z[i]
  den <- H[i] * hh_size[i]

  S <- sum(S_num)/sum(den)
  I <- sum(I_num)/sum(den)
  R <- sum(R_num)/(den)

  # Initialize all households to susceptible
  # initial(H[]) <- if (x[i] == hh_size[i] && y[i] == 0 && z[i] == 0) 1 else 0
  initial(H[]) <- 1/n_states

  deriv(H[]) <- gamma*


  # Differential equations
  deriv(H[]) <-
    # Recovery
    gamma * (
      - y[i] * H[i]
      + sum(if (x[j] == x[i] && y[j] == y[i] + 1 && z[j] == z[i] - 1 && hh_size[j] == hh_size[i]) (y[j]) * H[j] else 0,
            j = 1:n_states)
    ) +

    # Within-household transmission
    tau * (
      - x[i] * y[i] * H[i]
      + sum(if (x[j] == x[i] + 1 && y[j] == y[i] - 1 && z[j] == z[i] && hh_size[j] == hh_size[i])
              (x[j]) * (y[j]) * H[j] else 0,
            j = 1:n_states)
    ) +

    # Between-household transmission
    beta * I_t * (
      - x[i] * H[i]
      + sum(if (x[j] == x[i] + 1 && y[j] == y[i] - 1 && z[j] == z[i] && hh_size[j] == hh_size[i])
              x[j] * H[j] else 0,
            j = 1:n_states)
    )
})

# Load household state definitions
# household_states <- read.csv("household_states.csv")
n_states <- nrow(household_states)

# Initialize model
mod <- household_model$new(
  n_states = n_states,
  x = household_states$x,
  y = household_states$y,
  z = household_states$z,
  hh_size = household_states$n,
  gamma = 1/5,
  tau = 0.05,
  beta = 0.3
)

# Simulate
times <- seq(0, 100, by = 1)
out <- mod$run(times)

# Summarize expected infections over time
I_by_time <- sapply(1:length(times), function(t) {
  sum(out[t, 2:(n_states + 1)] * household_states$y)
})

plot(times, I_by_time, type = "l", ylab = "Expected Infected Individuals", xlab = "Time")
