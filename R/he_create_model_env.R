#' Create hydroepix model environment
#'
#' @return environment with no parent environment
#' @export
#'
he_create_model_env <- function() {
  # This function ensures the empty environment is the parent
  # This ensures the variable search path is limited to this environment
  return(rlang::new_environment())
}
