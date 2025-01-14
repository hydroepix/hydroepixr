test_that("internal simulation environment variables are initialized", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  expect_no_error(he_initialize_internal_simulation_vars(test_environment))
  expect_true(is.null(test_environment$outbreak_detected_last))
  expect_true(is.null(test_environment$outbreak_detected))
  expect_true(is.null(test_environment$depopulation_queue))
  expect_true(is.null(test_environment$being_depopulated))
  expect_true(is.na(test_environment$index_farm))
  expect_true(is.null(test_environment$iteration))
  expect_true(is.null(test_environment$infected_farm_nums))
  expect_equal(test_environment$g_time, 0)
})
