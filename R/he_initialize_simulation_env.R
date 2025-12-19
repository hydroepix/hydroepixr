#' Initialize simulation environment with necessary variables
#'
#' @param simulation_env environment in which simulation variables are stored
#' @param species_info data frame of species information
#' @param output_dir file path where the infected net pen information output should
#'    be stored
#' @param model_run_id identifier for this run of the model
#' @param infected_net_pen_output_file_name name of the file in which the infected
#'    net pen information should be stored
#' @param clinically_infected_prop the proportion of animals which will enter
#'    the clinical stage upon infection, which is the same as 1 minus the
#'    proportion of animals which will enter the subclinical stage upon
#'    infection
#' @param simulation_n identifies which simulation of the model run this
#'    this environment belongs to
#'
#' @return NA
#' @export
#' @importFrom stats runif
#'
he_initialize_simulation_env <-
  function(
    simulation_env,
    species_info,
    output_dir,
    model_run_id,
    infected_net_pen_output_file_name,
    clinically_infected_prop,
    simulation_n
  ) {
    # Define output parameters at the simulation level
    simulation_env$output_dir <- output_dir
    simulation_env$simulation_n <- simulation_n
    simulation_env$infected_net_pen_output_file_name <-
      paste(
        model_run_id,
        simulation_n,
        infected_net_pen_output_file_name,
        sep = "_"
      )

    simulation_env$clinically_infected_prop <- clinically_infected_prop

    # Create data frame and file to store infected net pen information
    simulation_env$infected_net_pen_info <-
      he_initialize_infected_net_pen_info(
        simulation_env$output_dir,
        simulation_env$infected_net_pen_output_file_name
      )
    # Initialize simulation-level net pen information variables
    #net_pen_info$susceptible_again <- rep(0, n_net_pens)
    #net_pen_info$survived <- rep(0, n_net_pens)
    #net_pen_info$infectiousness <- rep(0, n_net_pens)
    #net_pen_info$infection_mode <- rep(0, n_net_pens)

    # Initialize matrices to store disease stage durations
    # TODO: This will need to be updated to accommodate and label different
    # diseases
    simulation_env$disease_stage_duration_matrices <- list(
      latent_duration = matrix(
        numeric(0),
        ncol = length(species_info$latent_dur_freq[[1]])
      ),
      subclinical_duration = matrix(
        numeric(0),
        ncol = length(species_info$subclinical_dur_freq[[1]])
      ),
      clinical_duration = matrix(
        numeric(0),
        ncol = length(species_info$clinical_dur_freq[[1]])
      )
    )
  }
