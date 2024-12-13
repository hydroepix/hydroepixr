test_that("check initialized default values for preemption output file", {
  test_environment <- rlang::new_environment()
  filepath <- withr::local_tempfile(pattern = "preempted",
                                    fileext = ".txt")
  he_initialize_preemption_output(test_environment, filepath)
  expect_equal(test_environment$preemption_output_file_name,
               "preempted.txt")
})

test_that("check initialized non-default values for preemption output file", {
  test_environment <- rlang::new_environment()
  test_environment$run_id <- 1
  filepath <- withr::local_tempfile(pattern = "preempted",
                                    fileext = ".txt")
  he_initialize_preemption_output(test_environment, filepath)
  expect_equal(test_environment$preemption_output_file_name,
               "1-preempted.txt")
})

test_that("check initialized values in preemption matrix", {
  test_environment <- rlang::new_environment()
  filepath <- withr::local_tempfile(pattern = "preempted",
                                    fileext = ".txt")
  he_initialize_preemption_output(test_environment, filepath)
  expect_equal(test_environment$preemption_output,
               matrix(numeric(0), ncol = 10))
})

# TODO: Tests to see whether the file is written?
# TODO: Tests to see whether the values in the file are as expected?
