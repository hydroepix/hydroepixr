test_that("check initialized default values for infected cage output file", {
  test_environment <- rlang::new_environment()
  filepath <- withr::local_tempfile(pattern = "infected_cages",
                                    fileext = ".txt")
  he_initialize_infected_cage_output(test_environment, filepath)
  expect_equal(test_environment$infected_output_file_name,
               "infected_cages.txt")
})

test_that("check initialized non-default values for infected cage output file", {
  test_environment <- rlang::new_environment()
  test_environment$run_id <- 1
  filepath <- withr::local_tempfile(pattern = "infected_cages",
                                    fileext = ".txt")
  he_initialize_infected_cage_output(test_environment, filepath)
  expect_equal(test_environment$infected_output_file_name,
               "1-infected_cages.txt")
})

test_that("check initialized values in infected cage matrix", {
  test_environment <- rlang::new_environment()
  filepath <- withr::local_tempfile(pattern = "depopulated_farms",
                                    fileext = ".txt")
  he_initialize_infected_cage_output(test_environment, filepath)
  expect_equal(test_environment$infected_cages,
               matrix(numeric(0), ncol = 10))
})

# TODO: Tests to see whether the file is written?
# TODO: Tests to see whether the values in the file are as expected?
