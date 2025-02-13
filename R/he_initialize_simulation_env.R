#' Initialize simulation environment with necessary variables
#'
#' @param environment environment in which simulation variables are stored
#' @param num_farms number of farms
#' @param farm_info data frame of farm and netpen information
#'
#' @return NA
#' @export
#' @importFrom stats runif
#'
he_initialize_simulation_env <- function(simulation_env, num_farms, farm_info, species_info) {
  # Initialize simulation-level farm information variables
  farm_info$susceptible_again <- rep(0, num_farms)
  farm_info$survived <- rep(0, num_farms)
  farm_info$infectiousness <- rep(0, num_farms)
  farm_info$infection_mode <- rep(0, num_farms)

  farm_info <- he_reset_simulation_env(environment,
                                       environment$num_farms,
                                       environment$farm_info)
  # move into simulation reset?
  farm_info$status <- init_status
  farm_info$time_infected <- init_time_infected

  # select index herds for next simulation?
  if (ignore_status) {
    # TODO: Select index farm based on index farm selection function
    environment$index_farm <- select_index_farm()
    # apply infected status to the index farm
    farm_info$status[environment$index_farm]
    farm_info$time_infected[environment$index_farm] <- 0

    # TODO: infect index farm with number of fish according to type of
    # initial infection
    if (index_direct) {
      # TODO: construct an infected farm
      # aInfHerd$addInf(indexHerd,
      #                 matrix(c(0, 1, 0),
      #                        byrow = TRUE,
      #                        ncol = 3,
      #                        nrow = length(indexHerd)),
      #                 1)
    } else {
      # TODO: construct an infected farm
      # aInfHerd$addInf(indexHerd,
      #                 matrix(c(1, 0, 0),
      #                        byrow = TRUE,
      #                        ncol = 3,
      #                        nrow = length(indexHerd)),
      #                 0)
    }
  }

  # TODO: Review these variables - what are these bay management area vars for?
  temp_bay_management <- round(runif(unique(farm_info$bay_management_id)))
  farm_info$bay_management_time <- rep(0, num_farms)
  farm_info$productive_time <- rep(0, num_farms)
  farm_info$farm_active <- rep(FALSE, num_farms)
  farm_info$bay_management_time <- temp_bay_management[farm_info$bay_management_id]
  farm_info$productive_time <-
    farm_info$bay_management_time - round(runif(length(farm_info$bay_management_time), 0, 120))

  # Set the index farm to be active
  index_farm_id <- farm_info$farm_id[index_farm]
  farm_info$productive_time[farm_info$farm_id %in% index_farm_id] <-
    round(runif(sum(farm_info$farm_id %in% index_farm_id), 0, 120))
  farm_info$productive_time[index_farm]

  farm_info
}
