test_that("within netpen transmission probability is zero when no animals are
          subclinically infected", {
  test_infected_netpen_info <- readRDS(paste0(
    test_data_filepath,
    "/infected_netpen_info_dummy_data_latent_only.rds"
  ))
  test_infection_prob <-
    he_calculate_within_netpen_infection_prob(test_infected_netpen_info)
  expect_equal(test_infection_prob, c(0, 0, 0))
})

test_that("within netpen transmission probability is calculated correctly for
          a single value, with no immune", {
  test_infected_netpen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_netpen_info_dummy_data_single_row_with_infection.rds"
    ))
  test_infection_prob <-
    he_calculate_within_netpen_infection_prob(test_infected_netpen_info)
  expected_infection_prob <- 1 - exp(-0.5 * (2500 / 25000))
  expect_equal(test_infection_prob, expected_infection_prob)
})

test_that("with netpen transmission is zero for netpens where all animals are either recovered or dead and calculated correctly for other netpens", {
  test_infected_netpen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_netpen_info_dummy_data_with_infection_and_resolved_netpen.rds"
    ))
  test_infection_prob <-
    he_calculate_within_netpen_infection_prob(
      test_infected_netpen_info
    )
  expected_infection_prob <- c(
    0,
    1 - exp(-0.5 * (15000 / 21000)),
    1 - exp(-0.5 * (15000 / 22000))
  )
  expect_equal(test_infection_prob, expected_infection_prob)
})

test_that("within netpen transmission probability is calculated correctly for
          multiple values, with some immune", {
  test_infected_netpen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_netpen_info_dummy_data_with_infection.rds"
    ))
  test_infection_prob <- he_calculate_within_netpen_infection_prob(
    test_infected_netpen_info
  )
  expected_infection_prob <- c(1 - exp(-0.5 * (4500 / 24500)), 0, 0)
  expect_equal(test_infection_prob, expected_infection_prob)
})
