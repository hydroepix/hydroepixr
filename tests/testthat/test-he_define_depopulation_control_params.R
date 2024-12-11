test_that("default depopulation parameters are stored in the environment", {
  test_environment <- rlang::new_environment()
  he_define_depopulation_control_params(test_environment)
  expect_equal(test_environment$depop_capacity, c(20000))
  expect_equal(test_environment$species_to_depop, c(1:18))
  expect_equal(test_environment$depop_farm_if_netpen_infected, FALSE)
})

test_that("user-defined depopulation parameters stored in the environment", {
  test_depop_capacity <- c(10000)
  test_species_to_depop <- c(1:15)
  test_depop_farm_if_netpen_infected <- TRUE
  test_environment <- rlang::new_environment()
  he_define_depopulation_control_params(test_environment,
                                        depop_capacity = test_depop_capacity,
                                        species_to_depop = test_species_to_depop,
                                        depop_farm_if_netpen_infected = test_depop_farm_if_netpen_infected)
  expect_equal(test_environment$depop_capacity, test_depop_capacity)
  expect_equal(test_environment$species_to_depop, test_species_to_depop)
  expect_equal(test_environment$depop_farm_if_netpen_infected, test_depop_farm_if_netpen_infected)
})
