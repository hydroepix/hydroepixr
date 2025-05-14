test_that("model environment variables are initialized, excluding between-farm
          transmission", {
  # Create test directory and test model environment
  test_environment <- rlang::new_environment()
  test_environment$verbose <- TRUE
  expect_no_error(he_initialize_model_env(
    test_environment,
    netpen_info_filepath = test_netpen_info_filepath,
    species_info_filepath = test_species_info_filepath
  ))

  # Check files are read in
  expect_true(exists("netpen_info", where = test_environment))
  expect_true(exists("species_info", where = test_environment))
  expect_false(exists("connectivity_matrix", where = test_environment))

  # Check internal model variables are initialized
  expect_true(is.na(test_environment$index_netpens))
  expect_true(is.null(test_environment$infected_netpens))

  # Check variables pulled from netpen_info_file have been initialized
  expect_true(exists("n_netpens", where = test_environment))
  expect_true(exists("rel_susceptibility", where = test_environment$netpen_info))
  expect_true(exists("within_netpen_transmission", where = test_environment$netpen_info))

  # Check netpen_info has been initialized with proper values from species_info
  expect_true(all(
    test_environment$netpen_info$rel_susceptibility == 1))
  # Confirm within netpen transmission distribution values are within the
  # expected range
  expect_true(all(test_environment$netpen_info$within_netpen_transmission >= 0.14))
  expect_true(all(test_environment$netpen_info$within_netpen_transmission <= 0.8))

  # Check number of netpens is stored correctly
  expect_equal(test_environment$n_netpens, 60)
})


