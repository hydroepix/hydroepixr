test_that("rpert generates distribution of correct size", {
  dist_output <- rpert(10, 0.14, 0.4, 0.8)
  expect_length(dist_output, 10)
})

between_min_and_max <- function(x, min, max) {
  x >= min & x <= max
}

test_that("rpert generates values between min and max", {
  n <- 10
  min <- 0.14
  max <- 0.8
  dist_output <- rpert(n, min, 0.4, max)
  dist_output_in_range <- sapply(dist_output, between_min_and_max, min, max)
  true_vec <- rep(TRUE, n)
  expect_equal(dist_output_in_range, true_vec)
})
