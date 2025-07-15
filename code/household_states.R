generate_household_state_table <- function(n_max = 8) {
  # Create all possible (x, y, z) combinations where x + y + z <= n_max
  states <- expand.grid(
    x = 0:n_max,
    y = 0:n_max,
    z = 0:n_max
  ) %>%
    dplyr::mutate(
      hh_size = x + y + z
    ) %>%
    dplyr::filter(hh_size <= n_max) %>%
    dplyr::filter(hh_size>0) %>% 
    dplyr::arrange(hh_size, x, y, z) %>%
    dplyr::mutate(
      state_index = dplyr::row_number()
    )

  # Helper function to get state index
  find_index <- function(x_, y_, z_) {
    idx <- states %>%
      dplyr::filter(x == x_, y == y_, z == z_) %>%
      dplyr::pull(state_index)
    if (length(idx) == 0) return(0) else return(idx)
  }

  # Compute transitions
  # states <- states %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(
  #     recover_to = if (y > 0) find_index(x, y - 1, z + 1) else 0,
  #     within_to  = if (x > 0) find_index(x - 1, y + 1, z) else 0,
  #     between_to = within_to
  #   ) %>%
  #   dplyr::ungroup()
  states <- states %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rec_index = if (z > 0 && y < n_max) find_index(x, y + 1, z - 1) else 0,
      inf_index  = if (y > 0 && x < n_max) find_index(x + 1, y - 1, z) else 0
    ) %>%
    dplyr::ungroup()

  return(states)
}
