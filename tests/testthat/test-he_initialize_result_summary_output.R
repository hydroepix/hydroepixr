test_that("check initialized default values for result summary output file", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  test_environment$filepath <- output_test_setup()
  # Check function runs without errors
  expect_no_error(
    he_initialize_result_summary_output(test_environment,
                                        test_environment$filepath)
  )
  # Check result summary output file name is initialized to the default value
  expect_equal(test_environment$result_summary_output_file_name,
               "result_summary.txt")
  # Check if file has been written according to file name
  expect_true(file.exists(file.path(test_environment$filepath,
                                    "result_summary.txt")))
})

test_that("check initialized non-default values for result summary output
          file", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  test_environment$filepath <- output_test_setup()
  test_environment$run_id <- 1
  result_summary_output_file_name <- "result_summary.txt"
  # Check function runs without errors
  expect_no_error(
    he_initialize_result_summary_output(
      test_environment,
      test_environment$filepath,
      result_summary_output_file_name
    )
  )
  # Check result summary output file name is initialized to the custom value
  expect_equal(test_environment$result_summary_output_file_name,
               "1-result_summary.txt")
  # Check if file has been written according to file name
  expect_true(file.exists(file.path(test_environment$filepath,
                                    "1-result_summary.txt")))
})

# TODO: Add tests for cases where run ID is provided but custom name is not
# and vice versa

test_that("check initialized values in result summary matrix", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  test_environment$filepath <- output_test_setup()

  # Check function runs without errors
  expect_no_error(
    he_initialize_result_summary_output(test_environment,
                                        test_environment$filepath)
  )
  # Check depopulation matrix is initialized to default values
  expect_equal(test_environment$result_summary_matrix_output,
               matrix(numeric(0), ncol = 10))
})
