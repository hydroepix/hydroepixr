#' Initialize data frame to store and track infected net pen information
#'
#' @param output_dir file path where the infected net pen information output should
#'    be stored
#' @param output_file_name name of the file in which the infected net pen information
#'    should be stored
#'
#' @return empty data frame with appropriate column headers and types to store
#'    infected net pen information
#'
he_initialize_infected_net_pen_info <- function(output_dir, output_file_name) {
  infected_net_pen_info <- data.frame(
    simulation_day = integer(),
    net_pen_id = integer(),
    farm_id = integer(),
    species_id = integer(),
    within_net_pen_transmission = double(),
    n_susceptible = integer(),
    n_latent = integer(),
    n_subclinical = integer(),
    n_clinical = integer(),
    n_recovered = integer(),
    n_dead = integer(),
    n_total = integer(),
    infection_origin = character(),
    day_infected = double()
  )
  he_write_output_cols(
    names(infected_net_pen_info),
    output_dir,
    output_file_name
  )
  return(infected_net_pen_info)
}
