#' Create hydroepix simulation environment
#'
#' @param model_env environment to be used as the environment parent, which
#'    should be the model environment in the hydroepix context
#'
#' @return environment with provided argument environment as a parent
#' @export
#'
he_create_simulation_env <- function(model_env) {
  # This function ensures the empty environment is the parent
  # This ensures the variable search path is limited to this environment
  rlang::new_environment(parent = model_env)
}
