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
    # Select an index farm for this simulation
    he_infect_index_farm()

  }

  # summarize aggregated results from all simulations
  # TODO: Reset simulation environment? or is this covered in initialization?
}

