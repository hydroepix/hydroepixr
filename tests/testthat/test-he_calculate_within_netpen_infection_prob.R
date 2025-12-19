test_that("within   transmission probability is zero when no animals are
          subclinically infected", {
  test_infected_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/infected_net_pen_info_dummy_data_latent_only.rds"
  ))
  test_infection_prob <-
    he_calculate_within_net_pen_infection_prob(test_infected_net_pen_info)
  expect_equal(test_infection_prob, c(0, 0, 0))
})

test_that("within net pen transmission probability is calculated correctly for
          a single value, with no immune", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_dummy_data_single_row_with_infection.rds"
    ))
  test_infection_prob <-
    he_calculate_within_net_pen_infection_prob(test_infected_net_pen_info)
  expected_infection_prob <- 1 - exp(-0.5 * (2500 / 25000))
  expect_equal(test_infection_prob, expected_infection_prob)
})

test_that("with net pen transmission is zero for net pens where all animals are either recovered or dead and calculated correctly for other net pens", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_dummy_data_with_infection_and_resolved_net_pen.rds"
    ))
  test_infection_prob <-
    he_calculate_within_net_pen_infection_prob(
      test_infected_net_pen_info
    )
  expected_infection_prob <- c(
    0,
    1 - exp(-0.5 * (15000 / 21000)),
    1 - exp(-0.5 * (15000 / 22000))
  )
  expect_equal(test_infection_prob, expected_infection_prob)
})

test_that("within net pen transmission probability is calculated correctly for
          multiple values, with some immune", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_dummy_data_with_infection.rds"
    ))
  test_infection_prob <- he_calculate_within_net_pen_infection_prob(
    test_infected_net_pen_info
  )
  expected_infection_prob <- c(1 - exp(-0.5 * (4500 / 24500)), 0, 0)
  expect_equal(test_infection_prob, expected_infection_prob)
})
