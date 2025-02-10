#' Initialize internal simulation variables
#'
#' @param environment simulation environment
#'
#' @return NA
#' @export
#'
he_initialize_internal_model_vars <- function(environment) {
  # Declare additional variables
  # TODO: distinction between these two variables?
  environment$outbreak_detected_last <-
    environment$outbreak_detected <- NULL
  # TODO: do we need both of these?
  # TODO: why is this NA when everything else is NULL?
  environment$index_farm <- NA
  environment$iteration <- NULL
  # TODO: is this the correctly interpretation of infHerdNums?
  environment$infected_farm_nums <- NULL
  # TODO: what does g stand for?
  environment$sim_day <- 0
  # TODO: Are these useful matrices to keep? If so, pull into separate functions?
}
