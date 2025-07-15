library(odin)

household_model <- odin::odin({
  # Parameters
  n <- user()
  gamma <- user()
  tau <- user()
  beta <- user()
  
  # Derived number of household states
  dim(H) <- c(n+1, n+1, n+1)
  dim(dH) <- c(n+1, n+1, n+1)

  # Derived variable: total infected in the population
  I_tot <- sum(H[i, j, k] * j, i = 0:n, j = 0:n, k = 0:n, condition = i + j + k == n)

  # Time-varying external prevalence (could be replaced with input data)
  I_t <- I_tot

  # Household state transitions
  initial(H[i, j, k]) <- if (i == n && j == 0 && k == 0) 1 else 0  # All households start as (n, 0, 0)

  dH[i, j, k] <-
    # Recovery: (i,j,k) → (i,j-1,k+1)
    gamma * (
      - j * H[i, j, k] +
      if (j < n && k > 0) (j + 1) * H[i, j + 1, k - 1] else 0
    ) +
    
    # Within-household transmission: (i,j,k) → (i-1,j+1,k)
    tau * (
      - i * j * H[i, j, k] +
      if (i < n && j > 0) (i + 1) * (j - 1) * H[i + 1, j - 1, k] else 0
    ) +
    
    # Between-household transmission: (i,j,k) → (i-1,j+1,k)
    beta * I_t * (
      - i * H[i, j, k] +
      if (i < n && j > 0) (i + 1) * H[i + 1, j - 1, k] else 0
    )
})

mod <- household_model$new(
  n = 4,
  gamma = 1/5,
  tau = 0.05,
  beta = 0.3
)

times <- seq(0, 100, by = 1)
out <- mod$run(times)

# Summarize infected fraction over time
infected <- sapply(0:4, function(j) {
  apply(out[ , grepl(paste0(", ", j, ","), colnames(out))], 1, function(row) {
    sum(as.numeric(row) * j)
  })
})

plot(times, infected[, 1], type = "l", ylab = "Expected infected per household")
