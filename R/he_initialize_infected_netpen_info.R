#' Initialize data frame to store and track infected netpen information
#'
#' @param output_dir file path where the infected netpen information output should
#'    be stored
#' @param output_file_name name of the file in which the infected netpen information
#'    should be stored
#'
#' @return empty data frame with appropriate column headers and types to store
#'    infected netpen information
#' @export
#'
he_initialize_infected_netpen_info <- function(output_dir,
                                        output_file_name) {
  infected_netpen_info <- data.frame(
    simulation_day = integer(),
    netpen_id = integer(),
    farm_id = integer(),
    species_id = integer(),
    within_netpen_transmission = double(),
    n_susceptible = integer(),
    n_latent = integer(),
    n_subclinical = integer(),
    n_clinical = integer(),
    n_immune = integer(),
    n_total = integer(),
    infection_status = integer(),
    infection_origin = character(),
    day_infected = double(),
    is_vaccinated = double()
  )
  he_write_output_cols(names(infected_netpen_info),
                          output_dir,
                          output_file_name)
  infected_netpen_info
}
