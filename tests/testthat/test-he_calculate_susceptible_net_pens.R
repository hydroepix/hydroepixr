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
  n_susceptible_net_pens <- he_calculate_susceptible_net_pens(
    test_farm_id,
    test_net_pen_info,
    test_infected_net_pen_info
  )
  expect_equal(n_susceptible_net_pens, 20)
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
  n_susceptible_net_pens <- he_calculate_susceptible_net_pens(
    test_farm_id,
    test_net_pen_info,
    test_infected_net_pen_info
  )
  expect_equal(n_susceptible_net_pens, 19)
})

test_that("correct number of susceptible net pens is calculated when all net pens are infected", {
  # TODO: create new infected net pen info file which contains all net pens
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
  n_susceptible_net_pens <- he_calculate_susceptible_net_pens(
    test_farm_id,
    test_net_pen_info,
    test_infected_net_pen_info
  )
  expect_equal(n_susceptible_net_pens, 0)
})
