he_engine <- function() {
  # TODO: Iterate over specified length of simulation
    # Create and initialize simulation environment
    #sim_env <- he_create_simulation_env(model_env)
    #he_initialize_simulation_env(sim_env)
      # Update simulation/farm status variables
        # farm status, time, outbreak detected, infectious farms, farm production time
      # Run simulation iteration
        # "updateHerds()"
        # "HElimitmovements()"
        # "SurvHerds()"
      # Update relative effect of measures on DC, IMC and ILC
      # Replaced by BCinf(), DBspread()
        # run function from "controlMethods" -> function representing a day??
        # run function from "InterMethods" -> function representing a day??
      # Identify infectious herds
      # Create new infections if there are infected farms
      # Summarize the "day"?
    # Summarize the "iteration"?
    # TODO: What is the difference between a day and an iteration?
    # Clean new infection methods? newInfMethods[[i]]$cleaninteration()
    # Garbage collect?

}
