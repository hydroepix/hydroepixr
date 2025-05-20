test_that("rpert generates distribution of correct size", {
  dist_output <- he_rpert(10, 0.14, 0.4, 0.8)
  expect_length(dist_output, 10)
})

test_that("rpert generates values between min and max", {
  n <- 10
  min <- 0.14
  max <- 0.8
  dist_output <- he_rpert(n, min, 0.4, max)
  dist_output_in_range <- sapply(dist_output, between_min_and_max, min, max)
  expect_true(all(dist_output_in_range))
})

#TODO: Option to set seed and test exact values?
