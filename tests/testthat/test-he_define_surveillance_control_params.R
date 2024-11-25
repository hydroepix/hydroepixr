test_that("default surveillance parameters are stored in the environment", {
  test_environment <- rlang::new_environment()
  he_define_surveillance_control_params(test_environment)
  expect_equal(test_environment$control_functions, c("SurvZone(label='SZ')"))
  expect_equal(test_environment$prob_diagnosis, 1)
  expect_equal(test_environment$mort_increase_factor_for_first_investigation, 2)
  expect_equal(test_environment$mort_increase_factor_for_subsequent_investigation, 2)
  expect_equal(test_environment$time_between_survey_days, 4)
  expect_equal(test_environment$max_duration_surveillance_zone_days, 50)
  expect_equal(test_environment$past_days_for_dead_animal_surveillance, 7)
  expect_equal(test_environment$daily_farm_survey_limit, 2)
  expect_equal(test_environment$output_surveillance_details, FALSE)
})

test_that("user-defined surveillance parameters stored in the environment", {
  test_control_functions = "none"
  test_prob_diagnosis = 0.75
  test_mort_increase_factor_for_first_investigation = 1.5
  test_mort_increase_factor_for_subsequent_investigation = 2.5
  test_time_between_survey_days = 3
  test_max_duration_surveillance_zone_days = 30
  test_past_days_for_dead_animal_surveillance = 5
  test_daily_farm_survey_limit = 3
  test_output_surveillance_details = TRUE
  test_environment <- rlang::new_environment()
  he_define_surveillance_control_params(test_environment,
                                        control_functions = test_control_functions,
                                        prob_diagnosis = test_prob_diagnosis,
                                        mort_increase_factor_for_first_investigation = test_mort_increase_factor_for_first_investigation,
                                        mort_increase_factor_for_subsequent_investigation = test_mort_increase_factor_for_subsequent_investigation,
                                        time_between_survey_days = test_time_between_survey_days,
                                        max_duration_surveillance_zone_days = test_max_duration_surveillance_zone_days,
                                        past_days_for_dead_animal_surveillance = test_past_days_for_dead_animal_surveillance,
                                        daily_farm_survey_limit = test_daily_farm_survey_limit,
                                        output_surveillance_details = test_output_surveillance_details)
  expect_equal(test_environment$control_functions, test_control_functions)
  expect_equal(test_environment$prob_diagnosis, test_prob_diagnosis)
  expect_equal(test_environment$mort_increase_factor_for_first_investigation, test_mort_increase_factor_for_first_investigation)
  expect_equal(test_environment$mort_increase_factor_for_subsequent_investigation, test_mort_increase_factor_for_subsequent_investigation)
  expect_equal(test_environment$time_between_survey_days, test_time_between_survey_days)
  expect_equal(test_environment$max_duration_surveillance_zone_days, test_max_duration_surveillance_zone_days)
  expect_equal(test_environment$past_days_for_dead_animal_surveillance, test_past_days_for_dead_animal_surveillance)
  expect_equal(test_environment$daily_farm_survey_limit, test_daily_farm_survey_limit)
  expect_equal(test_environment$output_surveillance_details, test_output_surveillance_details)
})
