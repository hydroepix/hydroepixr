test_that("correct number of susceptible net pens is calculated when no net pens are infected", {
  test_farm_id <- 1
  test_infected_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/infected_net_pen_info_initialized.rds"
  ))
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_susceptible_net_pens <- he_retrieve_susceptible_net_pens(
    test_farm_id,
    test_net_pen_info,
    test_infected_net_pen_info
  )
  expected_susceptible_net_pens <- readRDS(paste0(
    test_data_filepath,
    "/net_pen_info_farm_1_all_susceptible.rds"
  ))
  expect_equal(test_susceptible_net_pens, expected_susceptible_net_pens)
})

test_that("correct number of susceptible net pens is calculated when some net pens are infected", {
  test_farm_id <- 1
  test_infected_net_pen_info <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_susceptible_net_pens <- he_retrieve_susceptible_net_pens(
    test_farm_id,
    test_net_pen_info,
    test_infected_net_pen_info
  )
  expected_susceptible_net_pens <- readRDS(paste0(
    test_data_filepath,
    "/net_pen_info_farm_1_some_susceptible.rds"
  ))
  expect_equal(test_susceptible_net_pens, expected_susceptible_net_pens)
})

test_that("correct number of susceptible net pens is calculated when all net pens are infected", {
  test_farm_id <- 1
  test_infected_net_pen_info <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_all_infected.rds"
    )
  )
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_susceptible_net_pens <- he_retrieve_susceptible_net_pens(
    test_farm_id,
    test_net_pen_info,
    test_infected_net_pen_info
  )
  expected_susceptible_net_pens <- readRDS(paste0(
    test_data_filepath,
    "/net_pen_info_farm_1_none_susceptible.rds"
  ))
  expect_equal(test_susceptible_net_pens, expected_susceptible_net_pens)
})
