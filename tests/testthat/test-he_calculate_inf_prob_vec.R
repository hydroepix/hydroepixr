test_that("probability matrix to infection probability vector calculation
          evaluates correctly", {
  test_prob_matrix <- matrix(c(0.25, 0, 0.5, 0, 0.5, 0.5, 0.5, 0.75, 0),
                             nrow = 3,
                             ncol = 3)
  expected_vec <- c(0.625, 0.75, 0.875)
  actual_vec <- he_calculate_inf_prob_vec(test_prob_matrix)
  expect_equal(actual_vec, expected_vec)
})
