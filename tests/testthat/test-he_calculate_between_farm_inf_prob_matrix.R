test_that("seaway distance infection probability matrix is calculated correctly", {
  test_dist_mat <-
    matrix(c(0, 2000, 7000, 2000, 0, 5000, 7000, 5000, 0),
           nrow = 3,
           ncol = 3)
  test_farm_ids <- c(1, 2, 3)
  test_farm_to_farm <- 0.5
  test_vaccine_efficacy <- 0
  test_farm_active <- 1
  test_farm_susceptibility <- 1
  test_farm_infectiousness <- 0.1
  expected_matrix <- matrix(
    c(
      0.0,
      0.001226264803904808,
      0.000100657944741062,
      0.001226264803904808,
      0.0,
      0.000273616662079663,
      0.000100657944741062,
      0.000273616662079663,
      0.0
    ),
    nrow = 3,
    ncol = 3
  )
  actual_matrix <- he_calculate_between_farm_inf_prob_matrix(
    test_dist_mat,
    test_farm_ids,
    test_farm_to_farm,
    test_vaccine_efficacy,
    test_farm_active,
    test_farm_susceptibility,
    test_farm_infectiousness,
    connectivity_matrix_type = "distance"
  )
  # Check that matrix is symmetrical (must also be square by definition)
  expect_true(isSymmetric(actual_matrix))
  # Check that the matrix has a zero diagonal
  expect_equal(diag(actual_matrix), rep(0.0, length(test_farm_ids)))
  # Check that the matrix has the correct values
  expect_equal(actual_matrix, expected_matrix)
})

test_that("particle contact hours infection probability matrix is calculated correctly", {
  # TODO: Check correct calculation with hydroconnectivity matrix
})
