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
    he_initialize_simulation_env(
      simulation_env,
      model_env$species_info,
      model_env$net_pen_info,
      model_env$output_dir,
      model_env$model_run_id,
      model_env$infected_net_pen_output_file_name,
      model_env$clinically_infected_prop,
      simulation_n
    )

    # Set random seed for the simulation
    model_env$random_seed <- he_set_random_seed(
      model_env$random_seed,
      simulation_n
    )
    # Select index net pens for this simulation
    model_env$index_net_pens <- he_select_index_net_pens(
      model_env$net_pen_info,
      model_env$index_net_pen_ids,
      model_env$index_farm_id
    )

    # Initialize infection tracking data object
    simulation_env$infected_net_pen_info <-
      he_initialize_infection(
        simulation_env$infected_net_pen_info,
        simulation_env,
        model_env$n_index_infected_min,
        model_env$n_index_infected_mode,
        model_env$n_index_infected_max,
        model_env$species_info,
        model_env$net_pen_info,
        model_env$index_net_pens,
        model_env$index_infection_stage,
        model_env$clinically_infected_prop
      )

    # loop over simulation days for as long as there are still net pens with an
    # active infection
    for (simulation_day in 1:model_env$max_outbreak_length) {
      if (model_env$verbose) {
        message(paste0("Simulation Day: ", simulation_day))
      }
      simulation_env$infected_net_pen_info <- he_simulate_day(
        simulation_env$infected_net_pen_info,
        simulation_env,
        simulation_day,
        model_env$species_info,
        model_env$verbose
      )

      infections_resolved <- he_check_if_infection_is_resolved(
        simulation_env$infected_net_pen_info
      )

      # Terminate simulation run if all infections have reached an endpoint
      if (infections_resolved) {
        break
      }
    }
  }
  # TODO: Generate output for results from all simulations
}
