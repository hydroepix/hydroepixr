test_that("default spread parameters are stored in the environment", {
  test_environment <- rlang::new_environment()
  he_define_spread_control_params(test_environment)
  expect_equal(test_environment$num_index_infected_min, 1)
  expect_equal(test_environment$num_index_infected_mode, 10)
  expect_equal(test_environment$num_index_infected_max, 100)
  expect_true(is.null(test_environment$index_netpen_ids))
  expect_true(is.null(test_environment$index_farm_id))
  expect_equal(test_environment$case_fatality_prop, 0.89)
  expect_equal(test_environment$days_dead_infectious, 2)
  expect_equal(test_environment$farm_to_farm, 0.42)
  expect_equal(test_environment$netpen_to_netpen, 0.052)
  expect_equal(test_environment$vaccine_efficacy, 0)
})

test_that("user-defined spread parameters stored in the environment", {
  test_index_netpen_ids <- c(1, 2, 3, 4, 5)
  test_num_index_infected_min <- 10
  test_num_index_infected_mode <- 10
  test_num_index_infected_max <- 10
  test_index_farm_id <- 1
  test_case_fatality_prop <- 0.75
  test_days_dead_infectious <- 1
  test_farm_to_farm <- 0.5
  test_netpen_to_netpen <- 0.025
  test_vaccine_efficacy <- 0.5
  test_environment <- rlang::new_environment()
  he_define_spread_control_params(test_environment,
                                  num_index_infected_min = test_num_index_infected_min,
                                  num_index_infected_mode = test_num_index_infected_mode,
                                  num_index_infected_max = test_num_index_infected_max,
                                  index_netpen_ids = test_index_netpen_ids,
                                  index_farm_id = test_index_farm_id,
                                  case_fatality_prop = test_case_fatality_prop,
                                  days_dead_infectious = test_days_dead_infectious,
                                  farm_to_farm = test_farm_to_farm,
                                  netpen_to_netpen = test_netpen_to_netpen,
                                  vaccine_efficacy = test_vaccine_efficacy)
  expect_equal(test_environment$num_index_infected_min, test_num_index_infected_min)
  expect_equal(test_environment$num_index_infected_mode, test_num_index_infected_mode)
  expect_equal(test_environment$num_index_infected_max, test_num_index_infected_max)
  expect_equal(test_environment$index_netpen_ids, test_index_netpen_ids)
  expect_equal(test_environment$index_farm_id, test_index_farm_id)
  expect_equal(test_environment$case_fatality_prop, test_case_fatality_prop)
  expect_equal(test_environment$days_dead_infectious, test_days_dead_infectious)
  expect_equal(test_environment$farm_to_farm, test_farm_to_farm)
  expect_equal(test_environment$netpen_to_netpen, test_netpen_to_netpen)
  expect_equal(test_environment$vaccine_efficacy, test_vaccine_efficacy)
})
