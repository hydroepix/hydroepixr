#' Create hydroepix simulation environment
#'
#' @return environment with no parent environment
#' @export
#'
he_create_simulation_env <- function() {
  # This function ensures the empty environment is the parent
  # This ensures the variable search path is limited to this environment
  rlang::new_environment()
}
