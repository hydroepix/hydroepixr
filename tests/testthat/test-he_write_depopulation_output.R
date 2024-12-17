test_that("depopulation output file generates error for missing depopulation matrix", {
  # Create test directory and environment
  temp_test_dir <- "temp_test_dir"
  withr::local_tempdir(pattern = temp_test_dir)
  test_environment <- rlang::new_environment()
  # Check for error
  expect_error(he_write_depopulation_output(test_environment,
                                            output_dir = temp_test_dir),
               regexp = "No depopulation matrix initialized.")
})

test_that("depopulation output file generates error for missing depopulation file name", {
  # Create test directory and environment
  temp_test_dir <- "temp_test_dir"
  withr::local_tempdir(pattern = temp_test_dir)
  test_environment <- rlang::new_environment()
  # Populate environment variables
  test_environment$depopulation_matrix_output <- matrix(numeric(0), ncol = 3)
  expect_error(he_write_depopulation_output(test_environment,
                                            output_dir = temp_test_dir),
               regexp = "No depopulation output file name initialized.")
})

test_that("depopulation output file is created in expected directory", {
  test_environment <- rlang::new_environment()
  temp_test_dir <- "temp_test_dir"
  test_environment$depopulation_matrix_output <- matrix(numeric(0), ncol = 3)
  test_environment$depopulation_output_file_name <- "depopulated_farms.txt"
  expected_filepath <- file.path(test_dir,
                                 test_environment$depopulation_output_file_name)
  withr::local_tempfile(
    {
      he_write_depopulation_output(test_environment, test_dir)
      expect_true(file.exists(expected_filepath))
      actual_data <- read.table(expected_filepath)
      expect_equal(actual_data, test_environment$depopulation_matrix_output)
    },
    tmpdir = temp_test_dir
  )
})

# TODO: Test that output file data is correctly written

# TODO: Check file output is overwritten on another call to the function

# TODO: Test that output file data is correctly appended, if append option selected
