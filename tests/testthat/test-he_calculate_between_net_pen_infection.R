test_that("between net pen transmission probability is zero when no animals are
          subclinically infected", {
  test_infected_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/infected_net_pen_info_dummy_data_latent_only.rds"
  ))
  test_net_pen_to_net_pen <- 0.6
  test_infection_prob <-
    he_calculate_between_net_pen_infection_prob(
      test_net_pen_to_net_pen,
      test_infected_net_pen_info
    )
  expect_equal(test_infection_prob, 0)
})

test_that("between net pen transmission probability is calculated correctly for
          a single value, with no immune", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_dummy_data_single_row_with_infection.rds"
    ))
  test_net_pen_to_net_pen <- 0.6
  test_infection_prob <-
    he_calculate_between_net_pen_infection_prob(
      test_net_pen_to_net_pen,
      test_infected_net_pen_info
    )
  expected_infection_prob <- 1 - exp(-0.6 * (2500 / 25000))
  expect_equal(test_infection_prob, expected_infection_prob)
})

test_that("between net pen transmission is not zero even where some net pen infections are resolved", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_dummy_data_with_infection_and_resolved_net_pen.rds"
    ))
  test_net_pen_to_net_pen <- 0.6
  test_infection_prob <-
    he_calculate_between_net_pen_infection_prob(
      test_net_pen_to_net_pen,
      test_infected_net_pen_info
    )
  expected_infection_prob <- 1 - exp(-0.6 * (30000 / 43000))
  expect_equal(test_infection_prob, expected_infection_prob)
})


test_that("between net pen transmission probability is calculated correctly for
          multiple net pens, with some immune", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_dummy_data_with_infection.rds"
    ))
  test_net_pen_to_net_pen <- 0.6
  test_infection_prob <-
    he_calculate_between_net_pen_infection_prob(
      test_net_pen_to_net_pen,
      test_infected_net_pen_info
    )
  expected_infection_prob <- 1 - exp(-0.6 * (4500 / 74500))
  expect_equal(test_infection_prob, expected_infection_prob)
})
