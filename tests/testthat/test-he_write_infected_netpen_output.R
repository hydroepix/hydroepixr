test_that("infected netpen output file generates error for missing infected
          netpen output data", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  # Check for error
  expect_error(
    he_write_infected_netpen_output(
      test_infected_netpen_output,
      test_infected_netpen_output_file_name,
      output_dir = temp_test_dir
    )
  )
})

test_that("infected netpen output file generates error for missing infected
          netpen file name", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  # Read in test data to output
  test_infected_netpen_output <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_netpen_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  # Check for error
  expect_error(
    he_write_infected_netpen_output(test_inf_netpen_output,
                               test_infected_netpen_output_file_name,
                               output_dir = temp_test_dir)
  )
})

test_that("infected netpen output file is created in expected directory", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  # Read in test data to output
  test_infected_netpen_output <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_netpen_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_infected_netpen_output_file_name <- "infected_netpens.csv"
  # Initialize test variable for comparison
  expected_filepath <- file.path(temp_test_dir, test_infected_netpen_output_file_name)
  he_write_infected_netpen_output(test_infected_netpen_output,
                             test_infected_netpen_output_file_name,
                             temp_test_dir)
  expect_true(file.exists(expected_filepath))
})

# TODO: Test that output file data is correctly written

# TODO: Check file output is overwritten on another call to the function

# TODO: Test that output file data is correctly appended, if append option selected
