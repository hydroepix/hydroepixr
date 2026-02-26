test_that("net change values are calculated correctly for a single net pen", {
  test_infection_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE), # latent
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE), # subclinical
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE) # clinical
  )
  test_n_newly_infected <- 1000
  test_clinically_infected_prop <- 0.5
  test_net_change <-
    he_calculate_net_change_in_infection_stage_count(
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  expected_net_change <- cbind(
    -1000, # susceptible
    1000 - 50, # latent
    25 - 10, # subclinical
    25 - 5, # clinical
    10, # recovered
    5
  ) # dead
  expect_equal(test_net_change, expected_net_change)
})

test_that("net change values are calculated correctly for a single net pen with
          unequal subclinical-clinical split", {
  test_infection_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE), # latent
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE), # subclinical
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE) # clinical
  )
  test_n_newly_infected <- 1000
  test_clinically_infected_prop <- 0.77
  test_net_change <-
    he_calculate_net_change_in_infection_stage_count(
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  expected_net_change <- cbind(
    -1000, # susceptible
    1000 - 50, # latent
    11 - 10, # subclinical
    39 - 5, # clinical
    10, # recovered
    5
  ) # dead
  expect_equal(test_net_change, expected_net_change)
})

test_that("net change values are calculated correctly for multiple net pens", {
  test_infection_stage_duration_matrices <- list(
    # latent
    matrix(
      c(100, 0, 0, 0, 0, 0, 0, 0, 50, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    # subclinical
    matrix(
      c(30, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    # clinical
    matrix(
      c(20, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    )
  )
  test_n_newly_infected <- c(2000, 1000)
  test_clinically_infected_prop <- 0.5
  test_net_change <-
    he_calculate_net_change_in_infection_stage_count(
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  expected_net_change <- rbind(
    cbind(-2000, 2000 - 100, 50 - 30, 50 - 20, 30, 20),
    cbind(-1000, 1000 - 50, 25 - 20, 25 - 10, 20, 10)
  )
  expect_equal(test_net_change, expected_net_change)
})
