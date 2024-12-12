test_that("default simulation control parameters are stored in the environment", {
  test_environment <- rlang::new_environment()

  he_define_simulation_control_params(test_environment)

  expect_equal(test_environment$n_simulations, 1)
  expect_equal(test_environment$run_id, NULL)
  expect_equal(test_environment$step_in_file, NULL)
  expect_equal(test_environment$max_outbreak_length, 365)
  expect_equal(test_environment$random_seed, -10)
  expect_equal(test_environment$ignore_disease_status_input, TRUE)
  expect_equal(test_environment$verbose, FALSE)
  expect_equal(test_environment$summary_function, "HEsum")
})

test_that("user-defined simulation control parameters stored in the environment", {
  test_n_simulations <- 1
  test_run_id <- 10
  test_step_in_file <- "test_file.txt"
  test_max_outbreak_length <- 730
  test_random_seed <- 123987234
  test_ignore_disease_status_input <- FALSE
  test_verbose <- TRUE
  test_summary_function <- "none"

  test_environment <- rlang::new_environment()
  he_define_simulation_control_params(test_environment,
                                      test_n_simulations,
                                      test_run_id,
                                      test_step_in_file,
                                      test_max_outbreak_length,
                                      test_random_seed,
                                      test_ignore_disease_status_input,
                                      test_verbose,
                                      test_summary_function)

  expect_equal(test_environment$n_simulations, test_n_simulations)
  expect_equal(test_environment$run_id, test_run_id)
  expect_equal(test_environment$step_in_file, test_step_in_file)
  expect_equal(test_environment$max_outbreak_length, test_max_outbreak_length)
  expect_equal(test_environment$random_seed, test_random_seed)
  expect_equal(test_environment$ignore_disease_status_input, test_ignore_disease_status_input)
  expect_equal(test_environment$verbose, test_verbose)
  expect_equal(test_environment$summary_function, test_summary_function)
})
