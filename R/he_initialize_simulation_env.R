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
he_initialize_simulation_env <- function(environment, num_farms, farm_info, species_info) {
  # Initialize simulation-level farm information variables
  farm_info$susceptible_again <- rep(0, num_farms)
  farm_info$survived <- rep(0, num_farms)
  farm_info$infectiousness <- rep(0, num_farms)
  farm_info$infection_mode <- rep(0, num_farms)

  # TODO: Check if this overlaps with he_initialize_farm_info?
  # iterate over species to set waiting periods and transmission probabilities
  # by species
  for (id in species_info$species_id) {
    farm_index <- farm_info$species_id == id
    num_type <- sum(farm_index)
    species_index <- species_info$species_id
    # intra-farm interaction rate
    # TODO
    # Assign k value to farm based on k value of species
    environment$farm_info$k[farm_index] <- species_info$k[species_index]
    # Assign relative susceptibility to farm based on relative susceptibility
    # of species
    environment$farm_info$rel_susceptibility[farm_index] <-
      environment$species_info$rel_susceptibility[species_index]
  }

  # assign reed-frost probability?
  environment$farm_info$p <- environment$farm_info$k
  environment$farm_info$p[environment$farm_info$p > 1] <- 1

  farm_info <- he_reset_simulation_env(environment,
                                       environment$num_farms,
                                       environment$farm_info)
  # move into simulation reset?
  farm_info$status <- init_status
  farm_info$time_infected <- init_time_infected

  # select index herds for next simulation?
  if (ignore_status) {
    # TODO: Select index farm based on index farm selection function
    environment$index_farm <- select_index_herd()
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
