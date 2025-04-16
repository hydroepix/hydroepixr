test_that("model environment variables are initialized", {
  # Create test directory and test model environment
  test_environment <- rlang::new_environment()
  temp_test_dir <- output_test_setup()
  test_environment$verbose <- TRUE
  expect_no_error(he_initialize_model_env(
    test_environment,
    farm_info_filepath = test_farm_info_filepath,
    species_info_filepath = test_species_info_filepath,
    connectivity_matrix_filepath = test_dist_matrix_filepath,
    output_filepath = temp_test_dir
  ))

  # Check files are read in
  expect_true(exists("farm_info", where = test_environment))
  expect_true(exists("species_info", where = test_environment))
  expect_true(exists("connectivity_matrix", where = test_environment))

  # Check internal model variables are initialized
  expect_true(is.na(test_environment$index_netpens))
  expect_equal(test_environment$simulation_num, 0)
  expect_true(is.null(test_environment$infected_netpens))
  expect_equal(test_environment$sim_day, 0)

  # Check variables pulled from farm_info_file have been initialized
  expect_true(exists("num_netpens", where = test_environment))
  expect_true(exists("rel_susceptibility", where = test_environment$farm_info))
  expect_true(exists("within_netpen_transmission", where = test_environment$farm_info))

  # Check farm_info has been initialized with proper values from species_info
  expect_true(all(
    test_environment$farm_info$rel_susceptibility == 1))
  # Confirm within netpen transmission distribution values are within the
  # expected range
  expect_true(all(test_environment$farm_info$within_netpen_transmission >= 0.14))
  expect_true(all(test_environment$farm_info$within_netpen_transmission <= 0.8))

  # Check number of farms is stored correctly
  expect_equal(test_environment$num_netpens, 60)
})


