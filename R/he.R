# Set up simulation environment for simulation variable management
simulation_env <- he_initialize_simulation_env()

# Define simulation parameters in the simulation environment
he_define_depopulation_control_params(simulation_env)
he_define_simulation_control_params(simulation_env)
he_define_spread_control_params(simulation_env)
he_define_surveillance_control_params(simulation_env)

he_run()


