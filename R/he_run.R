# TODO: Rename to he_run_model?
he_run <- function(model_env) {
  # Create simulation environment, will be recycled for each simulation
  simulation_env <- he_create_simulation_env(model_env)
  # Iterate over the specified number of simulations
  for (simulation in n_simulations) {
    if (verbose) {
      cat("Simulation ", simulation, "\n")
    }
    # Initialize simulation environment
    he_initialize_simulation_environment(simulation_env)
    # Set random seed for the simulation
    random_seed <- he_set_random_seed(random_seed_input, simulation)
    # Select index netpens for this simulation
    model_env$index_netpens <- he_select_index_netpens(model_env$farm_info,
                                                       model_env$index_netpen_ids,
                                                       model_env$index_farm_id)

  }

  # summarize aggregated results from all simulations
  # TODO: Reset simulation environment? or is this covered in initialization?
}

