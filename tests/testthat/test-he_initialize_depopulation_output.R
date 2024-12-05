test_that("check initialized default values for depopulation output file", {
  test_environment <- rlang::new_environment()
  filepath <- withr::local_tempfile(pattern = "depopulated_farms",
                                    fileext = ".txt")
  he_initialize_depopulation_output(test_environment, filepath)
  expect_equal(test_environment$depopulation_output_file_name,
               "depopulated_farms.txt")
})

test_that("check initialized non-default values for depopulation output file", {
  test_environment <- rlang::new_environment()
  test_environment$run_id <- 1
  filepath <- withr::local_tempfile(pattern = "depopulated_farms",
                                    fileext = ".txt")
  he_initialize_depopulation_output(test_environment, filepath)
  expect_equal(test_environment$depopulation_output_file_name,
               "1-depopulated_farms.txt")
})

test_that("check initialized values in depopulation matrix", {
  test_environment <- rlang::new_environment()
  filepath <- withr::local_tempfile(pattern = "depopulated_farms",
                                    fileext = ".txt")
  he_initialize_depopulation_output(test_environment, filepath)
  expect_equal(test_environment$depopulation_matrix_output,
               matrix(numeric(0), ncol = 3))
})
