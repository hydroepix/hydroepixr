output_test_setup <- function() {
  temp_test_dir <- tempdir()
  withr::defer(unlink(temp_test_dir, recursive = TRUE),
               teardown_env())
  temp_test_dir
}
