test_that("between netpen transmission probability is zero when no animals are
          subclinically infected", {
  test_infected_netpen_info <- readRDS(paste0(
    test_data_filepath,
    "/infected_netpen_info_dummy_data_latent_only.rds"
  ))
  test_netpen_to_netpen <- 0.6
  test_infection_prob <-
    he_calculate_between_netpen_infection_prob(
      test_netpen_to_netpen,
      test_infected_netpen_info
    )
  expect_equal(test_infection_prob, 0)
})

test_that("between netpen transmission probability is calculated correctly for
          a single value, with no immune", {
  test_infected_netpen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_netpen_info_dummy_data_single_row_with_infection.rds"
    ))
  test_netpen_to_netpen <- 0.6
  test_infection_prob <-
    he_calculate_between_netpen_infection_prob(
      test_netpen_to_netpen,
      test_infected_netpen_info
    )
  expected_infection_prob <- 1 - exp(-0.6 * (2500 / 25000))
  expect_equal(test_infection_prob, expected_infection_prob)
})

test_that("between netpen transmission is not zero even where some netpen infections are resolved", {
  test_infected_netpen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_netpen_info_dummy_data_with_infection_and_resolved_netpen.rds"
    ))
  test_netpen_to_netpen <- 0.6
  test_infection_prob <-
    he_calculate_between_netpen_infection_prob(
      test_netpen_to_netpen,
      test_infected_netpen_info
    )
  expected_infection_prob <- 1 - exp(-0.6 * (30000 / 43000))
  expect_equal(test_infection_prob, expected_infection_prob)
})


test_that("between netpen transmission probability is calculated correctly for
          multiple netpens, with some immune", {
  test_infected_netpen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_netpen_info_dummy_data_with_infection.rds"
    ))
  test_netpen_to_netpen <- 0.6
  test_infection_prob <-
    he_calculate_between_netpen_infection_prob(
      test_netpen_to_netpen,
      test_infected_netpen_info
    )
  expected_infection_prob <- 1 - exp(-0.6 * (4500 / 74500))
  expect_equal(test_infection_prob, expected_infection_prob)
})
