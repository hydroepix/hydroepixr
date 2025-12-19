test_that("correct result when infection is not resolved", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_multi_farm_infection.rds"
    ))

  expect_false(he_check_if_infection_is_resolved(test_infected_net_pen_info))
})

test_that("correct result when infection is resolved", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_multi_farm_infection_resolved.rds"
    ))
  expect_true(he_check_if_infection_is_resolved(test_infected_net_pen_info))
})
