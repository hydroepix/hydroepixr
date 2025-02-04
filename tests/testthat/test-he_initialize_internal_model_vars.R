test_that("internal model environment variables are initialized", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  expect_no_error(he_initialize_internal_model_vars(test_environment))
  expect_true(is.null(test_environment$outbreak_detected_last))
  expect_true(is.null(test_environment$outbreak_detected))
  expect_true(is.na(test_environment$index_farm))
  expect_true(is.null(test_environment$iteration))
  expect_true(is.null(test_environment$infected_farm_nums))
  expect_equal(test_environment$g_time, 0)
})
