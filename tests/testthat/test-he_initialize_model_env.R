test_that("model environment variables are initialized", {
  # Create test directory and test model environment
  test_environment <- rlang::new_environment()
  temp_test_dir <- output_test_setup()
  test_environment$verbose <- TRUE
  expect_no_error(he_initialize_model_env(
    test_environment,
    farm_info_filepath = test_farm_info_filepath,
    species_info_filepath = test_species_info_filepath,
    dist_mat_filepath = test_dist_mat_filepath,
    output_filepath = temp_test_dir
  ))

  # Check files are read in
  expect_true(exists("farm_info", where = test_environment))
  expect_true(exists("species_info", where = test_environment))
  expect_true(exists("dist_mat", where = test_environment))

  # Check internal model variables are initialized
  expect_no_error(he_initialize_internal_model_vars(test_environment))
  expect_true(is.null(test_environment$outbreak_detected_last))
  expect_true(is.null(test_environment$outbreak_detected))
  expect_true(is.null(test_environment$depopulation_queue))
  expect_true(is.null(test_environment$being_depopulated))
  expect_true(is.na(test_environment$index_farm))
  expect_true(is.null(test_environment$iteration))
  expect_true(is.null(test_environment$infected_farm_nums))
  expect_equal(test_environment$g_time, 0)

  # Check variables pulled from farm_info_file have been initialized
  expect_true(exists("num_farms", where = test_environment))
  expect_true(exists("susceptible", where = test_environment$farm_info))
  expect_true(exists("k", where = test_environment$farm_info))
  expect_true(exists("depop_eligible", where = test_environment$farm_info))

  # Check default outputs are initialized
  # Infected netpen output
  expect_true(exists("inf_netpen_matrix_output", where = test_environment))
  expect_true(exists("inf_netpen_output_file_name", where = test_environment))
  # Result summary output
  expect_true(exists("result_summary_matrix_output", where = test_environment))
  expect_true(exists("result_summary_output_file_name", where = test_environment))

  # Check nondefault outputs are not initialized
  # Survey output
  expect_false(exists("surveyed_matrix_output", where = test_environment))
  expect_false(exists("survey_output_file_name", where = test_environment))
  # Depopulation output
  expect_false(exists("depop_matrix_output", where = test_environment))
  expect_false(exists("depop_output_file_name", where = test_environment))
  # Preemptive depopulation output
  expect_false(exists("preemptive_depop_matrix_output", where = test_environment))
  expect_false(exists("preemptive_depop_output_file_name", where = test_environment))
})

test_that("additional model output variables are initialized", {
  # Create test directory and test model environment
  test_environment <- rlang::new_environment()
  temp_test_dir <- output_test_setup()
  test_environment$verbose <- TRUE
  expect_no_error(he_initialize_model_env(
    test_environment,
    farm_info_filepath = test_farm_info_filepath,
    species_info_filepath = test_species_info_filepath,
    dist_mat_filepath = test_dist_mat_filepath,
    output_filepath = temp_test_dir,
    detailed_survey_output = TRUE,
    detailed_depop_output = TRUE,
    detailed_preemptive_depop_output = TRUE
  ))
  # Check nondefault outputs are initialized
  # Survey output
  expect_true(exists("surveyed_matrix_output", where = test_environment))
  expect_true(exists("survey_output_file_name", where = test_environment))
  # Depopulation output
  expect_true(exists("depop_matrix_output", where = test_environment))
  expect_true(exists("depop_output_file_name", where = test_environment))
  # Preemptive depopulation output
  expect_true(exists("preemptive_depop_matrix_output", where = test_environment))
  expect_true(exists("preemptive_depop_output_file_name", where = test_environment))
})
