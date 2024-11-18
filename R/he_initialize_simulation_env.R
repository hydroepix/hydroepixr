he_initialize_simulation_env <- function() {
  # Set the parent as the empty environment to limit the var search path
  simulation_env <- rlang::env(parent = emptyenv())
}
