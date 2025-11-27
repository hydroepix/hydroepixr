test_that("prevalence is zero when no animals are infectious", {
  test_infected_netpen_info <- readRDS(paste0(
    test_data_filepath,
    "/infected_netpen_info_dummy_data_latent_only.rds"
  ))
  test_prevalence <-
    he_calculate_prevalence(test_infected_netpen_info)
  expect_equal(test_prevalence, c(0, 0, 0))
})

test_that("prevalence is zero for netpens where all animals are either recovered or dead and calculated correctly for other netpens", {
  test_infected_netpen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_netpen_info_dummy_data_with_infection_and_resolved_netpen.rds"
    ))
  test_prevalence <-
    he_calculate_prevalence(test_infected_netpen_info)
  expect_equal(test_prevalence, c(0, 15000 / 21000, 15000 / 22000))
})

test_that("prevalence is calculated correctly for a single infected netpen with infectious animals and non-immune animals", {
  test_infected_netpen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_netpen_info_dummy_data_single_row_with_infection.rds"
    ))
  test_prevalence <-
    he_calculate_prevalence(test_infected_netpen_info)
  expected_prevalence <- 2500 / 25000
  expect_equal(test_prevalence, expected_prevalence)
})
