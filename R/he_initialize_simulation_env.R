#' Initialize simulation environment with necessary variables
#'
#' @param simulation_env environment in which simulation variables are stored
#' @param num_netpens number of netpens
#' @param farm_info data frame of farm and netpen information
#' @param species_info data frame of species information
#'
#' @return NA
#' @export
#' @importFrom stats runif
#'
he_initialize_simulation_env <- function(simulation_env,
                                         num_netpens,
                                         farm_info,
                                         species_info,
                                         index_netpens) {
  # Initialize simulation-level farm information variables
  farm_info$susceptible_again <- rep(0, num_netpens)
  farm_info$survived <- rep(0, num_netpens)
  farm_info$infectiousness <- rep(0, num_netpens)
  farm_info$infection_mode <- rep(0, num_netpens)

  farm_info <- he_reset_simulation_env(simulation_env,
                                       num_netpens,
                                       farm_info)


  # select index herds for next simulation?
  # if (ignore_status) {
  #   # TODO: Select index farm based on index farm selection function
  #   environment$index_farm <- select_index_farm()
  #   # apply infected status to the index farm
  #   farm_info$status[simulation_env$index_farm]
  #   farm_info$time_infected[simulation_env$index_farm] <- 0
  #
  #   # TODO: infect index farm with number of fish according to type of
  #   # initial infection
  #   if (index_direct) {
  #     # TODO: construct an infected farm
  #     # aInfHerd$addInf(indexHerd,
  #     #                 matrix(c(0, 1, 0),
  #     #                        byrow = TRUE,
  #     #                        ncol = 3,
  #     #                        nrow = length(indexHerd)),
  #     #                 1)
  #   } else {
  #     # TODO: construct an infected farm
  #     # aInfHerd$addInf(indexHerd,
  #     #                 matrix(c(1, 0, 0),
  #     #                        byrow = TRUE,
  #     #                        ncol = 3,
  #     #                        nrow = length(indexHerd)),
  #     #                 0)
  #   }
  # }
  farm_info
}
