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
