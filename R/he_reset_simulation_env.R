#' Reset the simulation environment at the end of a simulation
#'
#' @param environment simulation environment to reset
#' @param num_farms number of farms in the model run
#' @param farm_info matrix of farm information
#'
#' @return matrix of farm information, reset to simulation start values
#' @export
#'
he_reset_simulation_env <- function(environment, num_farms, farm_info) {
  environment$sim_day <- 0
  environment$outbreak_detected_last <- environment$outbreak_detected <- FALSE

  farm_info
}
