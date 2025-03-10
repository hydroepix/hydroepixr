#' Run hydroepix model
#'
#' @param model_env environment containing variables for all essential hydroepix
#'    model parameters
#'
#' @export
#'
he_run_model <- function(model_env) {
  # TODO: Check that all required variables are initialized in the model env
  # Create simulation environment, will be recycled for each simulation
  simulation_env <- he_create_simulation_env(model_env)
  # Iterate over the specified number of simulations
  for (simulation_num in n_simulations) {
    # Inside here is analogous to HEengine.R
    if (verbose) {
      cat("Simulation ", simulation, "\n")
    }
    # Initialize simulation environment
    he_initialize_simulation_environment(simulation_env)
    # Set random seed for the simulation
    model_env$random_seed <- he_set_random_seed(random_seed_input, simulation)
    # Select index netpens for this simulation
    model_env$index_netpens <- he_select_index_netpens(model_env$farm_info,
                                                       model_env$index_netpen_ids,
                                                       model_env$index_farm_id)

    # TODO: Fill in arguments
    he_initialize_index_infection()

    # loop over simulation days for as long as there are farms that are not
    # in an infection stage?
    for (simulation_day in max_outbreak_length) {
      model_env$simulation
    }

  }

  # summarize aggregated results from all simulations
  # TODO: Reset simulation environment? or is this covered in initialization?
}

