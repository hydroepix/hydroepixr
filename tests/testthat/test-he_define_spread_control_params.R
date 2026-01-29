test_that("non-numeric n_index_infected_min generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_min = "one"
    ),
    regexp = "Error: n_index_infected_min value must be a numeric value greater than 0"
  )
})

test_that("n_index_infected_min less than 1 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_min = 0
    ),
    regexp = "Error: n_index_infected_min value must be a numeric value greater than 0"
  )
})

test_that("non-numeric n_index_infected_mode generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_mode = "twenty"
    ),
    regexp = "Error: n_index_infected_mode value must be a numeric value greater than 0"
  )
})

test_that("n_index_infected_mode less than 1 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_mode = 0
    ),
    regexp = "Error: n_index_infected_mode value must be a numeric value greater than 0"
  )
})

test_that("non-numeric n_index_infected_max generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_max = "two thousand"
    )
  )
})

test_that("n_index_infected_max less than 1 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_max = 0
    ),
    regexp = "Error: n_index_infected_max value must be a numeric value greater than 0"
  )
})

test_that("n_index_infected_min greater than n_index_infected_mode generates error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_min = 10,
      n_index_infected_mode = 1
    ),
    regexp = "Error: n_index_infected_min value must be smaller than n_index_infected_mode and n_index_infected_max"
  )
})

test_that("n_index_infected_min greater than n_index_infected_max generates error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_min = 10,
      n_index_infected_max = 1
    ),
    regexp = "Error: n_index_infected_min value must be smaller than n_index_infected_mode and n_index_infected_max"
  )
})

test_that("n_index_infected_mode greater than n_index_infected_max generates error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      n_index_infected_mode = 10,
      n_index_infected_max = 1
    ),
    regexp = "Error: n_index_infected_mode must be smaller than n_index_infected_max"
  )
})

test_that("invalid index_infection_stage generates error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      index_infection_stage = "subclinical"
    ),
    regexp = "Error: subclinical is not a valid value for index_infection_stage. Valid values are 'subclinical-clinical split' or 'latent'"
  )
})

test_that("non-numeric clinically_infected_prop value generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      clinically_infected_prop = "high"
    ),
    regexp = "Error: clinically_infected_prop value must be a numeric value between 0 and 1"
  )
})

test_that("clinically_infected_prop value less than zero generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      clinically_infected_prop = -1
    ),
    regexp = "Error: clinically_infected_prop value must be a numeric value between 0 and 1"
  )
})

test_that("clinically_infected_prop value greater than 1 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      clinically_infected_prop = 1.1
    ),
    regexp = "Error: clinically_infected_prop value must be a numeric value between 0 and 1"
  )
})

test_that("clinically_infected_prop value less than 0.05 generates a warning", {
  test_environment <- rlang::new_environment()
  expect_warning(
    he_define_spread_control_params(
      test_environment,
      clinically_infected_prop = 0
    ),
    regexp = "Warning: clinically_infected_prop value should typically be between 0.05 and 0.9"
  )
})

test_that("clinically_infected_prop value greater than 0.9 generates a warning", {
  test_environment <- rlang::new_environment()
  expect_warning(
    he_define_spread_control_params(
      test_environment,
      clinically_infected_prop = 1
    ),
    regexp = "Warning: clinically_infected_prop value should typically be between 0.05 and 0.9"
  )
})

test_that("non-numeric net_pen_to_net_pen value generates error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      net_pen_to_net_pen = "low"
    ),
    regexp = "Error: net_pen_to_net_pen value must be a numeric value between 0 and 1"
  )
})

test_that("net_pen_to_net_pen value less than 0 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      net_pen_to_net_pen = -1
    ),
    regexp = "Error: net_pen_to_net_pen value must be a numeric value between 0 and 1"
  )
})

test_that("net_pen_to_net_pen value greater than 1 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      net_pen_to_net_pen = 1.1
    ),
    regexp = "Error: net_pen_to_net_pen value must be a numeric value between 0 and 1"
  )
})

test_that("net_pen_to_net_pen value less than 0.01 generates a warning", {
  test_environment <- rlang::new_environment()
  expect_warning(
    he_define_spread_control_params(
      test_environment,
      net_pen_to_net_pen = 0.001
    ),
    regexp = "Warning: net_pen_to_net_pen value should typically be between 0.01 and 0.1"
  )
})

test_that("net_pen_to_net_pen value greater than 0.1 generates a warning", {
  test_environment <- rlang::new_environment()
  expect_warning(
    he_define_spread_control_params(
      test_environment,
      net_pen_to_net_pen = 0.2
    ),
    regexp = "Warning: net_pen_to_net_pen value should typically be between 0.01 and 0.1"
  )
})

test_that("non-numeric vaccine_efficacy value generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      vaccine_efficacy = "medium"
    ),
    regexp = "Error: vaccine_efficacy value must be a numeric value between 0 and 1"
  )
})

test_that("vaccine_efficacy value smaller than 0 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      vaccine_efficacy = -1
    ),
    regexp = "Error: vaccine_efficacy value must be a numeric value between 0 and 1"
  )
})

test_that("vaccine_efficacy value greater than 1 generates an error", {
  test_environment <- rlang::new_environment()
  expect_error(
    he_define_spread_control_params(
      test_environment,
      vaccine_efficacy = 1.1
    ),
    regexp = "Error: vaccine_efficacy value must be a numeric value between 0 and 1"
  )
})

test_that("vaccine_efficacy value greater than 0.6 generates a warning", {
  test_environment <- rlang::new_environment()
  expect_warning(
    he_define_spread_control_params(
      test_environment,
      vaccine_efficacy = 0.9
    ),
    regexp = "Warning: vaccine_efficacy values should typically not be above 0.6"
  )
})

test_that("default spread parameters are stored in the environment", {
  test_environment <- rlang::new_environment()
  he_define_spread_control_params(test_environment)
  expect_equal(test_environment$n_index_infected_min, 1)
  expect_equal(test_environment$n_index_infected_mode, 10)
  expect_equal(test_environment$n_index_infected_max, 100)
  expect_true(is.null(test_environment$index_net_pen_ids))
  expect_true(is.null(test_environment$index_farm_id))
  expect_equal(
    test_environment$index_infection_stage,
    "subclinical-clinical split"
  )
  expect_equal(test_environment$clinically_infected_prop, 0.89)
  expect_equal(test_environment$days_dead_infectious, 2)
  expect_equal(test_environment$farm_to_farm, 0.42)
  expect_equal(test_environment$net_pen_to_net_pen, 0.052)
  expect_equal(test_environment$vaccine_efficacy, 0)
})

test_that("user-defined spread parameters stored in the environment", {
  test_index_net_pen_ids <- c(1, 2, 3, 4, 5)
  test_n_index_infected_min <- 10
  test_n_index_infected_mode <- 10
  test_n_index_infected_max <- 10
  test_index_farm_id <- 1
  test_index_infection_stage <- "latent"
  test_clinically_infected_prop <- 0.75
  test_days_dead_infectious <- 1
  test_farm_to_farm <- 0.5
  test_net_pen_to_net_pen <- 0.025
  test_vaccine_efficacy <- 0
  test_environment <- rlang::new_environment()
  he_define_spread_control_params(
    test_environment,
    n_index_infected_min = test_n_index_infected_min,
    n_index_infected_mode = test_n_index_infected_mode,
    n_index_infected_max = test_n_index_infected_max,
    index_net_pen_ids = test_index_net_pen_ids,
    index_farm_id = test_index_farm_id,
    index_infection_stage = test_index_infection_stage,
    clinically_infected_prop = test_clinically_infected_prop,
    days_dead_infectious = test_days_dead_infectious,
    farm_to_farm = test_farm_to_farm,
    net_pen_to_net_pen = test_net_pen_to_net_pen,
    vaccine_efficacy = test_vaccine_efficacy
  )
  expect_equal(test_environment$n_index_infected_min, test_n_index_infected_min)
  expect_equal(
    test_environment$n_index_infected_mode,
    test_n_index_infected_mode
  )
  expect_equal(test_environment$n_index_infected_max, test_n_index_infected_max)
  expect_equal(test_environment$index_net_pen_ids, test_index_net_pen_ids)
  expect_equal(test_environment$index_farm_id, test_index_farm_id)
  expect_equal(
    test_environment$index_infection_stage,
    test_index_infection_stage
  )
  expect_equal(
    test_environment$clinically_infected_prop,
    test_clinically_infected_prop
  )
  expect_equal(test_environment$days_dead_infectious, test_days_dead_infectious)
  expect_equal(test_environment$farm_to_farm, test_farm_to_farm)
  expect_equal(test_environment$net_pen_to_net_pen, test_net_pen_to_net_pen)
  expect_equal(test_environment$vaccine_efficacy, test_vaccine_efficacy)
})

test_that("clinically_inf_prop is correctly modified by vaccine_efficacy", {
  test_index_net_pen_ids <- c(1, 2, 3, 4, 5)
  test_n_index_infected_min <- 10
  test_n_index_infected_mode <- 10
  test_n_index_infected_max <- 10
  test_index_farm_id <- 1
  test_index_infection_stage <- "latent"
  test_clinically_infected_prop <- 0.8
  test_days_dead_infectious <- 1
  test_farm_to_farm <- 0.5
  test_net_pen_to_net_pen <- 0.025
  test_vaccine_efficacy <- 0.5
  test_effective_clinically_infected_prop <- 0.8 * (1 - 0.5)
  test_environment <- rlang::new_environment()
  he_define_spread_control_params(
    test_environment,
    n_index_infected_min = test_n_index_infected_min,
    n_index_infected_mode = test_n_index_infected_mode,
    n_index_infected_max = test_n_index_infected_max,
    index_net_pen_ids = test_index_net_pen_ids,
    index_farm_id = test_index_farm_id,
    index_infection_stage = test_index_infection_stage,
    clinically_infected_prop = test_clinically_infected_prop,
    days_dead_infectious = test_days_dead_infectious,
    farm_to_farm = test_farm_to_farm,
    net_pen_to_net_pen = test_net_pen_to_net_pen,
    vaccine_efficacy = test_vaccine_efficacy
  )
  expect_equal(test_environment$n_index_infected_min, test_n_index_infected_min)
  expect_equal(
    test_environment$n_index_infected_mode,
    test_n_index_infected_mode
  )
  expect_equal(test_environment$n_index_infected_max, test_n_index_infected_max)
  expect_equal(test_environment$index_net_pen_ids, test_index_net_pen_ids)
  expect_equal(test_environment$index_farm_id, test_index_farm_id)
  expect_equal(
    test_environment$index_infection_stage,
    test_index_infection_stage
  )
  expect_equal(test_environment$days_dead_infectious, test_days_dead_infectious)
  expect_equal(test_environment$farm_to_farm, test_farm_to_farm)
  expect_equal(test_environment$net_pen_to_net_pen, test_net_pen_to_net_pen)
  expect_equal(test_environment$vaccine_efficacy, test_vaccine_efficacy)
  expect_equal(
    test_environment$clinically_infected_prop,
    test_effective_clinically_infected_prop
  )
})
