test_that("seaway distance access distance matrix values correctly", {
  test_dist_mat <- matrix(c(0, 2000, 7000, 2000, 0, 5000, 7000, 5000, 0),
                          nrow = 3,
                          ncol = 3)
  test_farm_id <- 1
  test_dist <- test_dist_mat[test_farm_id,]
  test_farm_to_farm <- 0.5
  test_vaccine_efficacy <- 0
  expected <- c(0.033333333, 0.012262648, 0.001006579)
  actual <- he_generate_seaway_dist_prob(test_dist,
                                         test_farm_to_farm,
                                         test_vaccine_efficacy)
  # Check the length returned matches the length inputted
  expect_length(actual, length(test_dist))
  expect_setequal(actual, expected)
})
