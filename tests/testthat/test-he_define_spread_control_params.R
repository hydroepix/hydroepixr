test_that("default spread parameters are stored in the environment", {
  test_environment <- rlang::new_environment()
  he_define_spread_control_params(test_environment)
  expect_equal(test_environment$new_infection_functions, c("BCSpread()", "DBinf()"))
  expect_equal(test_environment$random_num_initially_infected_fish, FALSE)
  expect_equal(test_environment$intrafarm_disease_transmission_model, "binomial chain")
  expect_equal(test_environment$index_farm_function, "selectIndexFarm")
  expect_equal(test_environment$index_farm_select, list(species = 1:18))
  expect_equal(test_environment$index_direct, FALSE)
  expect_equal(test_environment$mort_threshold_for_first_investigation, 0.00255)
  expect_equal(test_environment$mort_threshold_for_subsequent_investigation, 0.00255)
  expect_equal(test_environment$case_fatality_prop, 0.89)
  expect_equal(test_environment$days_dead_infectious, 2)
  expect_equal(test_environment$farm_to_farm, 0.42)
  expect_equal(test_environment$cage_to_cage, 0.052)
  expect_equal(test_environment$vaccine_efficacy, 0)
})

test_that("user-defined spread parameters stored in the environment", {
  test_new_infection_functions = "none"
  test_random_num_initially_infected_fish = TRUE
  test_intrafarm_disease_transmission_model = "reed-frost"
  test_index_farm_function = "none"
  test_index_farm_select = c()
  test_index_direct = TRUE
  test_mort_threshold_for_first_investigation = 0.003
  test_mort_threshold_for_subsequent_investigation = 0.0015
  test_case_fatality_prop = 0.75
  test_days_dead_infectious = 1
  test_farm_to_farm = 0.5
  test_cage_to_cage = 0.025
  test_vaccine_efficacy = 0.5
  test_environment <- rlang::new_environment()
  he_define_spread_control_params(test_environment,
                                  new_infection_functions = test_new_infection_functions,
                                  random_num_initially_infected_fish = test_random_num_initially_infected_fish,
                                  intrafarm_disease_transmission_model = test_intrafarm_disease_transmission_model,
                                  index_farm_function = test_index_farm_function,
                                  index_farm_select = test_index_farm_select,
                                  index_direct = test_index_direct,
                                  mort_threshold_for_first_investigation = test_mort_threshold_for_first_investigation,
                                  mort_threshold_for_subsequent_investigation = test_mort_threshold_for_subsequent_investigation,
                                  case_fatality_prop = test_case_fatality_prop,
                                  days_dead_infectious = test_days_dead_infectious,
                                  farm_to_farm = test_farm_to_farm,
                                  cage_to_cage = test_cage_to_cage,
                                  vaccine_efficacy = test_vaccine_efficacy)
  expect_equal(test_environment$new_infection_functions, test_new_infection_functions)
  expect_equal(test_environment$random_num_initially_infected_fish, test_random_num_initially_infected_fish)
  expect_equal(test_environment$intrafarm_disease_transmission_model, test_intrafarm_disease_transmission_model)
  expect_equal(test_environment$index_farm_function, test_index_farm_function)
  expect_equal(test_environment$index_farm_select, test_index_farm_select)
  expect_equal(test_environment$index_direct, test_index_direct)
  expect_equal(test_environment$mort_threshold_for_first_investigation, test_mort_threshold_for_first_investigation)
  expect_equal(test_environment$mort_threshold_for_subsequent_investigation, test_mort_threshold_for_subsequent_investigation)
  expect_equal(test_environment$case_fatality_prop, test_case_fatality_prop)
  expect_equal(test_environment$days_dead_infectious, test_days_dead_infectious)
  expect_equal(test_environment$farm_to_farm, test_farm_to_farm)
  expect_equal(test_environment$cage_to_cage, test_cage_to_cage)
  expect_equal(test_environment$vaccine_efficacy, test_vaccine_efficacy)
})
