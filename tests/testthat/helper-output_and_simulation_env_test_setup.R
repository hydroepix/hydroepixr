output_and_simulation_env_test_setup <- function(test_environment,
                                                 test_species_info,
                                                 test_output_file_name) {
  temp_test_dir <- output_test_setup()
  he_initialize_simulation_env(
    test_environment,
    test_species_info,
    temp_test_dir,
    model_run_id = "testmodel",
    test_output_file_name,
    simulation_num = 1
  )
  # Return the temp directory
  temp_test_dir
}
