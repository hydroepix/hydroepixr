test_that("depopulation output file is created", {
  test_environment <- rlang::new_environment()
  test_dir <- "temp_test_dir"
  withr::local_tempfile(
    test_dir,
    {
      test_environment$filepath <-
        file.path(test_dir, "depopulated_farms.txt")
      expect_no_error(he_write_depopulation_output(test_environment,
                                                   append = FALSE))
      expect_true(file.exists(test_environment$filepath))
    }
  )
})
#
# test_that("depopulation file is appended", {
#   test_environment <- rlang::new_environment()
#   filepath <- withr::local_tempfile(pattern = "depopulated_farms",
#                                     fileext = ".txt")
#   he_initialize_depopulation_output(test_environment, filepath)
#   expect_equal(test_environment$depopulation_output_file_name,
#                "depopulated_farms.txt")
# })
