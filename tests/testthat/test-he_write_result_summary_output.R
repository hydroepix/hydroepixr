test_that("result summary output file generates error for missing
          result summary matrix", {
            # Create test directory
            temp_test_dir <- output_test_setup()
            # Check for error
            expect_error(
              he_write_result_summary_output(result_summary_matrix,
                                             result_summary_file_name,
                                             output_dir = temp_test_dir)
            )
          })

test_that("result summary output file generates error for missing
          result summary file name", {
            # Create test directory
            temp_test_dir <- output_test_setup()
            # Populate environment variables
            result_summary_matrix <- matrix(numeric(0), ncol = 10)
            # Check for error
            expect_error(
              he_write_result_summary_output(result_summary_matrix,
                                             result_summary_file_name,
                                             output_dir = temp_test_dir)
            )
          })

test_that("result_summary output file is created in expected directory", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  # Populate environment variables
  result_summary_matrix <- matrix(numeric(0), ncol = 10)
  result_summary_file_name <- "result_summary.txt"
  # Initialize test variable for comparison
  expected_filepath <- file.path(temp_test_dir, result_summary_file_name)
  he_write_result_summary_output(result_summary_matrix,
                                 result_summary_file_name,
                                 temp_test_dir)
  expect_true(file.exists(expected_filepath))
})

# TODO: Test that output file data is correctly written

# TODO: Check file output is overwritten on another call to the function

# TODO: Test that output file data is correctly appended, if append option selected
