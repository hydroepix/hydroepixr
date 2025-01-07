test_that("preemptive depopulation output file generates error for missing
          preemptive depopulation matrix", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  # Check for error
  expect_error(he_write_depopulation_output(preemptive_depop_matrix,
                                            preemptive_depop_file_name,
                                            output_dir = temp_test_dir))
})

test_that("preemptive depopulation output file generates error for missing
          preemptive depopulation file name", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  # Populate environment variables
  preemptive_depop_matrix <- matrix(numeric(0), ncol = 3)
  # Check for error
  expect_error(he_write_depopulation_output(preemptive_depop_matrix,
                                            preemptive_depop_file_name,
                                            output_dir = temp_test_dir))
})

test_that("preemptive depopulation output file is created in expected
          directory", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  # Populate environment variables
  preemptive_depop_matrix <- matrix(numeric(0), ncol = 3)
  preemptive_depop_file_name <- "preemptive_depop.txt"
  # Initialize test variable for comparison
  expected_filepath <- file.path(temp_test_dir, preemptive_depop_file_name)
  he_write_depopulation_output(preemptive_depop_matrix,
                               preemptive_depop_file_name,
                               temp_test_dir)
  expect_true(file.exists(expected_filepath))
})

# TODO: Test that output file data is correctly written

# TODO: Check file output is overwritten on another call to the function

# TODO: Test that output file data is correctly appended, if append option selected
