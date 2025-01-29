test_that("seaway distance values are calculated correctly", {
  test_dist <-
    matrix(c(0, 2000, 7000, 2000, 0, 5000, 7000, 5000, 0), ncol = 3, nrow = 3)
  test_farm_id <- 1
  test_farm_to_farm <- 0.5
  test_vaccine_efficacy <- 0
  expected <- c(0.0333333333333333,
                0.0122626480390481,
                0.00100657944741062)
  actual <- he_generate_seaway_dist_prob(test_farm_id,
                                         test_dist,
                                         test_farm_to_farm,
                                         test_vaccine_efficacy)
  # Check for only numeric values
  expect_true(all(sapply(actual, is.numeric)))
  # Check the length returned matches the length inputted
  expect_length(actual, ncol(test_dist))
  # Check the set of values output is equal
  expect_equal(actual, expected)
})
