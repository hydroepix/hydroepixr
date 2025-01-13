test_that("simulation environment variables are initialized", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  temp_test_dir <- output_test_setup()
  test_environment$verbose <- TRUE
  expect_no_error(he_initialize_simulation_env(
    test_environment,
    farm_info_filepath = test_farm_info_filepath,
    species_info_filepath = test_species_info_filepath,
    dist_mat_filepath = test_dist_mat_filepath,
    output_filepath = temp_test_dir
  ))
})
