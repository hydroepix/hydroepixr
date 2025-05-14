test_that("specified columns are correctly written to file", {
    # Create test directory
    temp_test_dir <- output_test_setup()
    # Read in test data to output
    test_col_names <- names(readRDS(
      paste0(
        test_data_filepath,
        "/infected_netpen_info_bay_x_with_multi_farm_infection.rds"
      )
    ))
    test_output_file_name <- "infected_netpens.csv"
    he_write_output_cols(test_col_names,
                            temp_test_dir,
                            test_output_file_name)
    test_output_file_data <- read.csv(file.path(temp_test_dir,
                                               test_output_file_name))
    expect_equal(names(test_output_file_data), test_col_names)
  })
