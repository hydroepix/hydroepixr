test_that("within netpen transmission probability is zero when no animals are
          subclinically infected", {
  test_infected_netpen_info <- readRDS(paste0(test_data_filepath,
                                       "/infected_netpen_info_dummy_data_latent_only.rds"))
  test_vaccine_efficacy <- 0
  test_infection_prob <-
    he_calculate_within_netpen_infection_prob(test_infected_netpen_info,
                                              test_vaccine_efficacy)
  expect_equal(test_infection_prob, c(0, 0, 0))
})

test_that("within netpen transmission probability is calculated correctly for
          a single value, with no immune, without vaccine", {
  test_infected_netpen_info <-
    readRDS(paste0(test_data_filepath,
                   "/infected_netpen_info_dummy_data_single_row_with_infection.rds"))
  test_vaccine_efficacy <- 0
  test_infection_prob <-
    he_calculate_within_netpen_infection_prob(test_infected_netpen_info,
                                              test_vaccine_efficacy)
  expected_infection_prob <- 1 - exp(-0.5 * (2500/25000))
  expect_equal(test_infection_prob, expected_infection_prob)
})

test_that("within netpen transmission probability is calculated correctly for
          multiple values, with some immune, without vaccine", {
  test_infected_netpen_info <-
    readRDS(paste0(test_data_filepath,
                   "/infected_netpen_info_dummy_data_with_infection.rds"))
  test_vaccine_efficacy <- 0
  test_infection_prob <-
    he_calculate_within_netpen_infection_prob(test_infected_netpen_info,
                                              test_vaccine_efficacy)
  expected_infection_prob <- c(1 - exp(-0.5 * (4500/24500)), 0, 0)
  expect_equal(test_infection_prob, expected_infection_prob)
})

test_that("within netpen transmission probability is calculated correctly for
          a single value, with vaccine", {
  # TODO
})

test_that("within netpen transmission probability is calculated correctly for
          multiple values, with vaccine", {
  # TODO
})
