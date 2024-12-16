test_that("check initialized default values for depopulation output file", {
  test_environment <- rlang::new_environment()
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "depopulated_farms.txt")
      expect_no_error(he_initialize_depopulation_output(test_environment,
                                                        test_environment$filepath))
      expect_true(file.exists(test_environment$filepath))
      expect_equal(test_environment$depopulation_output_file_name,
                   "depopulated_farms.txt")
    }
  )
})

test_that("check initialized non-default values for depopulation output file", {
  test_environment <- rlang::new_environment()
  test_environment$run_id <- 1
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "depopulated_farms.txt")
      expect_no_error(he_initialize_depopulation_output(test_environment,
                                                        test_environment$filepath))
      he_initialize_depopulation_output(test_environment, filepath)
      expect_equal(test_environment$depopulation_output_file_name,
                   "1-depopulated_farms.txt")
    }
  )
})

test_that("check initialized values in depopulation matrix", {
  test_environment <- rlang::new_environment()
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "depopulated_farms.txt")
      expect_no_error(he_initialize_depopulation_output(test_environment,
                                                        test_environment$filepath))
      he_initialize_depopulation_output(test_environment, filepath)
      expect_equal(test_environment$depopulation_matrix_output,
                   matrix(numeric(0), ncol = 3))
    }
  )
})

# TODO: Tests to see whether the file is written?
# TODO: Tests to see whether the values in the file are as expected?
