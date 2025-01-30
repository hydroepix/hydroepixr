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
  environment$g_time <- 0
  farm_info$time_to_tagged_for_depop <- rep(Inf, num_farms)
  farm_infO$diagnosis_time <- rep(Inf, num_farms)
  farm_info$diagnosed <- rep(F, num_farms)
  farm_info$diag_surv <- rep(F, num_farms)

  environment$outbreak_detected_last <- environment$outbreak_detected <- FALSE

  environment$depopulation_queue <- matrix(numeric(0), ncol = 2)
  environment$being_depopulated <- matrix(numeric(0), ncol = 2)
  environment$zone_queue <- matrix(numeric(0), ncol = 2)

  farm_info$time_to_remove_zone <- rep(Inf, num_farms)

  farm_info
}
