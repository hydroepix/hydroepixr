test_that("net change values are calculated correctly for a single netpen", {
  test_disease_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE), # latent
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE), # subclinical
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE) # clinical
  )
  test_n_newly_infected <- 1000
  test_clinically_infected_prop <- 0.5
  test_net_change <-
    he_calculate_net_change_in_disease_stage_count(
      test_disease_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  expected_net_change <- cbind(-1000, 1000 - 50, 25 - 10, 25 - 10, 10, 10)
  expect_equal(test_net_change, expected_net_change)
})

test_that("net change values are calculated correctly for multiple netpens", {
  test_disease_stage_duration_matrices <- list(
    matrix(c(100, 0, 0, 0, 0, 0, 0, 0,
             50, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0,
             20, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0,
             20, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE)
  )
  test_n_newly_infected <- c(2000, 1000)
  test_clinically_infected_prop <- 0.5
  test_net_change <-
    he_calculate_net_change_in_disease_stage_count(
      test_disease_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  expected_net_change <- rbind(cbind(-2000, 2000 - 100, 50 - 25, 50 - 25, 25, 25),
                               cbind(-1000, 1000 - 50, 25 - 20, 25 - 20, 20, 20))
  expect_equal(test_net_change, expected_net_change)
})
