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
  # I_t <- sum(H[i] * y[i], i = 1:n_states) / sum(H[i] * hh_size[i], i = 1:n_states)
  # I_t <- sum(H[i] * y[i]) / sum(H[i] * hh_size[i])
  dim(infected_contrib) <- n_states
  infected_contrib[] <- H[i] * y[i]

  dim(pop_contrib) <- n_states
  pop_contrib[] <- H[i] * hh_size[i]

  I_t_numerator <- sum(infected_contrib)
  I_t_denominator <- sum(pop_contrib)

  I_t <- I_t_numerator / I_t_denominator

  # Initialize all households to susceptible
  initial(H[]) <- if (x[i] == hh_size[i] && y[i] == 0 && z[i] == 0) 1 else 0

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