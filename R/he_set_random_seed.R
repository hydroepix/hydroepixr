#' Set random seed
#'
#' @param seed numeric value for the seed
#' @param simulation_n simulation number, based on the number of simulations
#'    the model is set to run
#'
#' @return seed value, variable by simulation number if seed value provided is
#'    negative, fixed otherwise
#'
he_set_random_seed <- function(seed, simulation_n) {
  if (seed < 0) {
    seed <- abs(seed) + simulation_n
  }
  return(seed)
}
