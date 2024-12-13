test_that("check initialized default values for infected netpen output file", {
  test_environment <- rlang::new_environment()
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "infected_netpens.txt")
      expect_no_error(he_initialize_depopulation_output(test_environment,
                                                        test_environment$filepath,
                                                        append = FALSE))
      he_initialize_infected_netpen_output(test_environment, filepath)
      expect_equal(test_environment$infected_output_file_name,
                   "infected_netpens.txt")
    }
  )
})

test_that("check initialized non-default values for infected netpen output file", {
  test_environment <- rlang::new_environment()
  test_environment$run_id <- 1
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "infected_netpens.txt")
      expect_no_error(he_initialize_depopulation_output(test_environment,
                                                        test_environment$filepath,
                                                        append = FALSE))
      he_initialize_infected_netpen_output(test_environment, filepath)
      expect_equal(test_environment$infected_output_file_name,
                   "1-infected_netpens.txt")
    }
  )
})

test_that("check initialized values in infected netpen matrix", {
  test_environment <- rlang::new_environment()
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "infected_netpens.txt")
      expect_no_error(he_initialize_depopulation_output(test_environment,
                                                        test_environment$filepath,
                                                        append = FALSE))
      he_initialize_infected_netpen_output(test_environment, filepath)
      expect_equal(test_environment$infected_netpens,
                   matrix(numeric(0), ncol = 10))
    }
  )
})

# TODO: Tests to see whether the file is written?
# TODO: Tests to see whether the values in the file are as expected?
