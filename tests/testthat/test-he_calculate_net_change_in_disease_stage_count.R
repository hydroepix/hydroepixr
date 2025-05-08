test_that("net change values are calculated correctly for a single netpen", {
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- 1000
  test_net_change <-
    he_calculate_net_change_in_disease_stage_count(
      test_disease_stage_duration_matrices,
      test_n_newly_infected
    )
  expected_net_change <- cbind(-1000, 1000 - 25, 25 - 10, 10 - 5, 5)
  expect_equal(test_net_change, expected_net_change)
})

test_that("net change values are calculated correctly for multiple netpens", {
  test_disease_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0,
             25, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0,
             10, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0,
             5, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE)
  )
  test_n_newly_infected <- c(2000, 1000)
  test_net_change <-
    he_calculate_net_change_in_disease_stage_count(
      test_disease_stage_duration_matrices,
      test_n_newly_infected
    )
  expected_net_change <- rbind(cbind(-2000, 2000 - 50, 50 - 25, 25 - 10, 10),
                               cbind(-1000, 1000 - 25, 25 - 10, 10 - 5, 5))
  expect_equal(test_net_change, expected_net_change)
})
