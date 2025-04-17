#' Initialize simulation environment with necessary variables
#'
#' @param simulation_env environment in which simulation variables are stored
#' @param species_info data frame of species information
#' @param output_dir file path where the infected farm information output should
#'    be stored
#' @param inf_netpen_output_file_name name of the file in which the infected
#'    farm information should be stored
#'
#' @return NA
#' @export
#' @importFrom stats runif
#'
he_initialize_simulation_env <-
  function(simulation_env,
           species_info,
           output_dir,
           inf_netpen_output_file_name) {

  # Create data frame and file to store infected farm and netpen information
  simulation_env$inf_farm_info <-
    he_initialize_inf_farm_info(output_dir, inf_netpen_output_file_name)
  # Initialize simulation-level farm information variables
  #farm_info$susceptible_again <- rep(0, num_netpens)
  #farm_info$survived <- rep(0, num_netpens)
  #farm_info$infectiousness <- rep(0, num_netpens)
  #farm_info$infection_mode <- rep(0, num_netpens)

  # Initialize matrices to store disease stage durations
  # TODO: This will need to be updated to accommodate and label different
  # diseases
  simulation_env$disease_stage_duration_matrices <- list(
    latent_duration =
      matrix(numeric(0), ncol = length(species_info$latent_dur_freq[[1]])),
    subclinical_duration =
      matrix(numeric(0), ncol = length(species_info$subclinical_dur_freq[[1]])),
    clinical_duration =
      matrix(numeric(0), ncol = length(species_info$clinical_dur_freq[[1]]))
  )
  simulation_env$sim_day <- 0
}
