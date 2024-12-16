test_that("check initialized default values for result summary output file", {
  test_environment <- rlang::new_environment()
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "result_summary_farms.txt")
      expect_no_error(he_initialize_result_summary_output(test_environment,
                                                        test_environment$filepath))
      he_initialize_result_summary_output(test_environment, filepath)
      expect_equal(test_environment$result_summary_file_name,
                   "result_summary.txt")
    }
  )
})

test_that("check initialized non-default values for result summary output file", {
  test_environment <- rlang::new_environment()
  test_environment$run_id <- 1
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "1-result_summary.txt")
      expect_no_error(he_initialize_result_summary_output(test_environment,
                                                          test_environment$filepath))
      he_initialize_result_summary_output(test_environment, filepath)
      expect_equal(test_environment$result_summary_file_name,
                   "1-result_summary.txt")
    }
  )
})

test_that("check initialized values in result summary output matrix", {
  test_environment <- rlang::new_environment()
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "result_summary.txt")
      expect_no_error(he_initialize_result_summary_output(test_environment,
                                                          test_environment$filepath))
      he_initialize_result_summary_output(test_environment, filepath)
      expect_equal(test_environment$result_summary,
                   matrix(numeric(0), ncol = 10))
    }
  )
})

# TODO: Tests to see whether the file is written?
# TODO: Tests to see whether the values in the file are as expected?
