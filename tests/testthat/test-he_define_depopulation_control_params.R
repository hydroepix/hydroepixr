test_that("default depopulation parameters are stored in the environment", {
  test_environment <- rlang::new_environment()
  he_define_depopulation_control_params(test_environment)
  expect_equal(test_environment$culling_capacity, c(20000))
  expect_equal(test_environment$cull_types, c(1:18))
  expect_equal(test_environment$cull_farm_if_cage_infection, FALSE)
})

test_that("user-defined depopulation parameters stored in the environment", {
  test_culling_capacity <- c(10000)
  test_cull_types <- c(1:15)
  test_cull_farm_if_cage_infection <- TRUE
  test_environment <- rlang::new_environment()
  he_define_depopulation_control_params(test_environment,
                                        culling_capacity = test_culling_capacity,
                                        cull_types = test_cull_types,
                                        cull_farm_if_cage_infection = test_cull_farm_if_cage_infection)
  expect_equal(test_environment$culling_capacity, test_culling_capacity)
  expect_equal(test_environment$cull_types, test_cull_types)
  expect_equal(test_environment$cull_farm_if_cage_infection, test_cull_farm_if_cage_infection)
})
