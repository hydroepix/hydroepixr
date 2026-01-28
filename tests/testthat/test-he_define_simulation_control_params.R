test_that("a non-numeric n_simulations value generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_simulation_control_params(
      test_environment,
      n_simulations = 0
    ),
    regexp = "Error: n_simulations value must be a numeric value greater than 0"
  )
})

test_that("a value of n_simulations less than 1 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_simulation_control_params(
      test_environment,
      n_simulations = "ten"
    ),
    regexp = "Error: n_simulations value must be a numeric value greater than 0"
  )
})

test_that("a value of n_simulations less than 10 generates a warning", {
  test_environment <- rlang::new_environment()
  expect_warning(
    he_define_simulation_control_params(
      test_environment,
      n_simulations = 1
    ),
    regexp = "Due to the inclusion of randomness in the model, running a small number of simulations will provide a less representative model. It is recommended to run several simulations and aggregate their results."
  )
})

test_that("a value of n_simulations greater than 1000 generates a warning", {
  test_environment <- rlang::new_environment()
  expect_warning(
    he_define_simulation_control_params(
      test_environment,
      n_simulations = 2000
    ),
    regexp = "Running a large number of simulations will take a large amount of time. It is recommended not to run more than 1000 simulations."
  )
})

test_that("a non-numeric max_outbreak_length value generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_simulation_control_params(
      test_environment,
      max_outbreak_length = "ten"
    ),
    regexp = "Error: max_outbreak_length must be numeric value greater than 0"
  )
})

test_that("a value of max_outbreak_length less than 1 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_simulation_control_params(
      test_environment,
      max_outbreak_length = 0
    ),
    regexp = "Error: max_outbreak_length must be numeric value greater than 0"
  )
})

test_that("a value of max_outbreak_length greater than 720 generates a warning", {
  test_environment <- rlang::new_environment()
  expect_warning(
    he_define_simulation_control_params(
      test_environment,
      max_outbreak_length = 1000
    ),
    regexp = "Simulations with a long max_outbreak_length will take a large amount of time to run and will not typically represent the length of time the animals will be in the water. Values of 720 days or less are recommended."
  )
})

test_that("a non-numeric random_seed value generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_simulation_control_params(
      test_environment,
      random_seed = "random"
    )
  )
})

test_that("non boolean value for ignore_disease_status_input generates error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_simulation_control_params(
      test_environment,
      ignore_disease_status_input = "false"
    )
  )
})

test_that("non boolean value for verbose generates error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_simulation_control(
      test_environment,
      verbose = 1
    )
  )
})

test_that("default simulation control parameters are stored in the environment", {
  test_environment <- rlang::new_environment()

  he_define_simulation_control_params(test_environment)

  expect_equal(test_environment$n_simulations, 10)
  expect_equal(test_environment$max_outbreak_length, 365)
  expect_equal(test_environment$random_seed, -10)
  expect_equal(test_environment$ignore_disease_status_input, TRUE)
  expect_equal(test_environment$verbose, FALSE)
})

test_that("user-defined simulation control parameters stored in the environment", {
  test_n_simulations <- 100
  test_max_outbreak_length <- 720
  test_random_seed <- 123987234
  test_ignore_disease_status_input <- FALSE
  test_verbose <- TRUE

  test_environment <- rlang::new_environment()
  he_define_simulation_control_params(
    test_environment,
    test_n_simulations,
    test_max_outbreak_length,
    test_random_seed,
    test_ignore_disease_status_input,
    test_verbose
  )

  expect_equal(test_environment$n_simulations, test_n_simulations)
  expect_equal(test_environment$max_outbreak_length, test_max_outbreak_length)
  expect_equal(test_environment$random_seed, test_random_seed)
  expect_equal(
    test_environment$ignore_disease_status_input,
    test_ignore_disease_status_input
  )
  expect_equal(test_environment$verbose, test_verbose)
})
