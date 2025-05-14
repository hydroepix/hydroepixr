#' Run hydroepix model
#'
#' @param model_env environment containing variables for all essential hydroepix
#'    model parameters
#'
#' @export
#'
he_run_model <- function(model_env) {
  # TODO: Check that all required variables are initialized in the model env
  # Iterate over the specified number of simulations
  for (simulation_n in model_env$n_simulations) {
    # Inside here is analogous to HEengine.R
    if (model_env$verbose) {
      cat("Simulation ", simulation_n, "\n")
    }
    # Create and initialize simulation environment
    simulation_env <- he_create_simulation_env(model_env)
    he_initialize_simulation_env(simulation_env,
                                 model_env$species_info,
                                 model_env$output_dir,
                                 model_env$model_run_id,
                                 model_env$infected_netpen_output_file_name,
                                 simulation_n)

    # Set random seed for the simulation
    model_env$random_seed <- he_set_random_seed(model_env$random_seed,
                                                simulation_n)
    # Select index netpens for this simulation
    model_env$index_netpens <- he_select_index_netpens(model_env$netpen_info,
                                                       model_env$index_netpen_ids,
                                                       model_env$index_farm_id)

    # Initialize infection tracking data object
    simulation_env$infected_netpen_info <-
      he_initialize_infection(simulation_env$infected_netpen_info,
                              simulation_env,
                              model_env$n_index_infected_min,
                              model_env$n_index_infected_mode,
                              model_env$n_index_infected_max,
                              model_env$species_info,
                              model_env$netpen_info,
                              model_env$index_netpens)

    # loop over simulation days for as long as there are still netpens with an
    # active infection
    for(simulation_day in 1:model_env$max_outbreak_length) {
      if (model_env$verbose) {
        message(paste0("Simulation Day: ", simulation_day))
      }
      simulation_env$infected_netpen_info <- he_simulate_day(
        simulation_env$infected_netpen_info,
        simulation_env,
        simulation_day,
        model_env$species_info,
        model_env$verbose
      )
      # TODO: Add termination condition in case where no animals are infected:
      #any(inf_netpen_info$infection_status %in% c(2, 3, 4))
    }
  }
  # TODO: Generate output for results from all simulations
}

