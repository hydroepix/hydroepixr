#' Initialize data frame to store and track infected farm information
#'
#' @param output_dir file path where the infected farm information output should
#'    be stored
#' @param output_file_name name of the file in which the infected farm information
#'    should be stored
#'
#' @return empty data frame with appropriate column headers and types to store
#'    infected farm and netpen information
#' @export
#'
he_initialize_inf_farm_info <- function(output_dir,
                                        output_file_name) {
  inf_farm_info <- data.frame(
    netpen_id = integer(),
    farm_id = integer(),
    species_id = integer(),
    within_netpen_transmission = double(),
    susceptible = integer(),
    latent = integer(),
    subclinical = integer(),
    clinical = integer(),
    immune = integer(),
    total = integer(),
    infection_status = integer(), #???
    latent_duration = double(), # Length of time in latent stage?
    subclinical_duration = double(), # Length of time in subclinical stage?
    clinical_time = double(), # time the herd showed clinical signs of infection
    time_of_diagnosis = double(),
    diagnosed = logical(),
    infected_by_direct_contact = character(),
    time_infected = double(), # appears to refer to the timestep of infection
    vaccinated = double()
  )
  he_write_output_columns(names(inf_farm_info),
                          output_dir,
                          output_file_name)
  inf_farm_info
}
