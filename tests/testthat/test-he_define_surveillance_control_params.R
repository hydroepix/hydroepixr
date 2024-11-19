test_that("default surveillance parameters are stored in the environment", {
  test_environment <- rlang::new_environment()
  he_define_surveillance_control_params(test_environment)
  expect_equal(test_environment$control_functions, c("SurvZone(label='SZ')"))
  expect_equal(test_environment$prob_diagnosis, 1)
  expect_equal(test_environment$mortality_increase_1, 2)
  expect_equal(test_environment$mortality_increase_2, 2)
  expect_equal(test_environment$time_between_visits_within_zone, 4)
  expect_equal(test_environment$protection_zone_duration, 50)
  expect_equal(test_environment$past_days_for_dead_animal_surveillance, 7)
  expect_equal(test_environment$daily_farm_survey_limit, 2)
  expect_equal(test_environment$output_surveillance_details, FALSE)
})

test_that("user-defined surveillance parameters stored in the environment", {
  test_control_functions = "none"
  test_prob_diagnosis = 0.75
  test_mortality_increase_1 = 1.5
  test_mortality_increase_2 = 2.5
  test_time_between_visits_within_zone = 3
  test_protection_zone_duration = 30
  test_past_days_for_dead_animal_surveillance = 5
  test_daily_farm_survey_limit = 3
  test_output_surveillance_details = TRUE
  test_environment <- rlang::new_environment()
  he_define_surveillance_control_params(test_environment,
                                        control_functions = test_control_functions,
                                        prob_diagnosis = test_prob_diagnosis,
                                        mortality_increase_1 = test_mortality_increase_1,
                                        mortality_increase_2 = test_mortality_increase_2,
                                        time_between_visits_within_zone = test_time_between_visits_within_zone,
                                        protection_zone_duration = test_protection_zone_duration,
                                        past_days_for_dead_animal_surveillance = test_past_days_for_dead_animal_surveillance,
                                        daily_farm_survey_limit = test_daily_farm_survey_limit,
                                        output_surveillance_details = test_output_surveillance_details)
  expect_equal(test_environment$control_functions, test_control_functions)
  expect_equal(test_environment$prob_diagnosis, test_prob_diagnosis)
  expect_equal(test_environment$mortality_increase_1, test_mortality_increase_1)
  expect_equal(test_environment$mortality_increase_2, test_mortality_increase_2)
  expect_equal(test_environment$time_between_visits_within_zone, test_time_between_visits_within_zone)
  expect_equal(test_environment$protection_zone_duration, test_protection_zone_duration)
  expect_equal(test_environment$past_days_for_dead_animal_surveillance, test_past_days_for_dead_animal_surveillance)
  expect_equal(test_environment$daily_farm_survey_limit, test_daily_farm_survey_limit)
  expect_equal(test_environment$output_surveillance_details, test_output_surveillance_details)
})
