test_that("prevalence is zero when no animals are infectious", {
  test_n_infectious <- c(0, 0, 0)
  test_n_not_immune <- c(25000, 25000, 25000)
  test_prevalence <-
    he_calculate_prevalence(test_n_infectious, test_n_not_immune)
  expect_equal(test_prevalence, c(0, 0, 0))
})

test_that("prevalence is zero for net pens where all animals are either recovered or dead and calculated correctly for other net pens", {
  test_n_infectious <- c(0, 15000, 15000)
  test_n_not_immune <- c(
    25000 - 10000 - 15000,
    25000 - 2000 - 2000,
    25000 - 1500 - 15000
  )
  test_prevalence <-
    he_calculate_prevalence(test_n_infectious, test_n_not_immune)
  expect_equal(
    test_prevalence,
    c(0, 15000 / (25000 - 2000 - 2000), 15000 / (25000 - 1500 - 15000))
  )
})

test_that("prevalence is calculated correctly for a single infected net pen with infectious animals and non-immune animals", {
  test_n_infectious <- 2500
  test_n_not_immune <- 25000
  test_prevalence <-
    he_calculate_prevalence(test_n_infectious, test_n_not_immune)
  expect_equal(test_prevalence, 2500 / 25000)
})
