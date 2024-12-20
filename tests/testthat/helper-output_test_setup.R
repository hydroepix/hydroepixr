output_test_setup <- function(temp_test_dir = "temp_test_dir") {
  dir.create(temp_test_dir)
}

output_test_teardown <- function(temp_test_dir = "temp_test_dir") {
  unlink(temp_test_dir, recursive = TRUE)
}
