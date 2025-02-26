test_that("rpoly2 generates correct number of bins", {
  dist_output <-
    he_rpoly2(50000,
              matrix(c(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125),
                     nrow = 1))
  expect_length(dist_output, 8)
})

test_that("rpoly2 generates values within zero and the provided sample size", {
  dist_output <-
    he_rpoly2(50000,
              matrix(c(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125),
                     nrow = 1))
  dist_output_in_range <- sapply(dist_output, between_min_and_max, 0, 50000)
  expect_true(all(dist_output_in_range))
})

#TODO: Option to set seed and test exact values?
