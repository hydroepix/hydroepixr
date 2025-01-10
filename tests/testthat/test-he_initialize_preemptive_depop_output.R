test_that("check initialized default values for preemptive depop output file", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  test_environment$filepath <- output_test_setup()
  # Check function runs without errors
  expect_no_error(
    he_initialize_preemptive_depop_output(test_environment,
                                          test_environment$filepath)
  )
  # Check depopulation output file name is initialized to the default value
  expect_equal(test_environment$preemptive_depop_output_file_name,
               "preemptive_depop.txt")
  # Check if file has been written according to file name
  expect_true(file.exists(file.path(test_environment$filepath,
                                    "preemptive_depop.txt")))
})

test_that("check initialized non-default values for preemptivedepop output
          file", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  test_environment$filepath <- output_test_setup()
  test_environment$run_id <- 1
  preemptive_depop_output_file_name <- "preemptive_depop.txt"
  # Check function runs without errors
  expect_no_error(
    he_initialize_preemptive_depop_output(
      test_environment,
      test_environment$filepath,
      preemptive_depop_output_file_name
    )
  )
  # Check depopulation output file name is initialized to the custom value
  expect_equal(test_environment$preemptive_depop_output_file_name,
               "1-preemptive_depop.txt")
  # Check if file has been written according to file name
  expect_true(file.exists(file.path(test_environment$filepath,
                                    "1-preemptive_depop.txt")))
})

test_that("check initialized values in preemptive depop matrix", {
  # Create test directory and test simulation environment
  test_environment <- rlang::new_environment()
  test_environment$filepath <- output_test_setup()
  # Check function runs without errors
  expect_no_error(
    he_initialize_preemptive_depop_output(test_environment,
                                          test_environment$filepath)
  )
  # Check depopulation matrix is initialized to default values
  expect_equal(test_environment$preemptive_depop_matrix_output,
               matrix(numeric(0), ncol = 3))
})
