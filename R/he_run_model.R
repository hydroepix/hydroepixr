#' Run hydroepix model
#'
#' @param model_env environment containing variables for all essential hydroepix
#'    model parameters
#' @param verbose logical indicating whether to produce verbose outputs
#'
#' @export
#'
he_run_model <- function(model_env, verbose = FALSE) {
  # TODO: Check that all required variables are initialized in the model env
  # Iterate over the specified number of simulations
  for (simulation_num in model_env$n_simulations) {
    # Inside here is analogous to HEengine.R
    if (verbose) {
      cat("Simulation ", simulation_num, "\n")
    }
    # Create and initialize simulation environment
    simulation_env <- he_create_simulation_env(model_env)
    he_initialize_simulation_env(simulation_env,
                                 model_env$species_info,
                                 model_env$output_dir,
                                 model_env$model_run_id,
                                 model_env$inf_netpen_output_file_name,
                                 simulation_num)

    # Set random seed for the simulation
    model_env$random_seed <- he_set_random_seed(random_seed_input,
                                                simulation_num)
    # Select index netpens for this simulation
    model_env$index_netpens <- he_select_index_netpens(model_env$farm_info,
                                                       model_env$index_netpen_ids,
                                                       model_env$index_farm_id)

    simulation_env$inf_farm_info <-
      he_initialize_infection(simulation_env$inf_farm_info,
                              simulation_env,
                              model_env$species_info,
                              model_env$farm_info,
                              model_env$index_netpens)

    # loop over simulation days for as long as there are still farms with an
    # active infection
    while(simulation_day < max_outbreak_length #&
          #any(inf_farm_info$infection_status %in% c(2, 3, 4))
          ) {
      he_simulate_day(inf_farm_info, simulation_env)
    }
    # TODO: Generate output for a simulation - entire inf farm info for now
    he_write_inf_netpen_output(inf_farm_info,
                               output_dir = "output")
  }
  # TODO: Generate output for results from all simulations
}

