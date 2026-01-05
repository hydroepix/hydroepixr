output_and_simulation_env_test_setup <- function(
  test_environment,
  test_species_info,
  test_net_pen_info,
  test_output_file_name
) {
  temp_test_dir <- output_test_setup()
  he_initialize_simulation_env(
    test_environment,
    test_species_info,
    test_net_pen_info,
    temp_test_dir,
    model_run_id = "testmodel",
    test_output_file_name,
    clinically_infected_prop = 0.6,
    simulation_n = 1
  )
  # Return the temp directory
  temp_test_dir
}
