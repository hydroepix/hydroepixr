test_that("new disease stage row is appended correctly", {
  withr::local_seed(100)
  test_empty_duration_matrix <- matrix(0, ncol = 8)
  test_disease_stage_distribution <-
    c(0.000001, 0.000001, 0.0001, 0.01, 0.1, 0.25, 0.339898, 0.3)
  test_n_animals_to_distribute <- 50000
  test_result_duration_matrix <- he_add_disease_stage_duration(
    test_empty_duration_matrix,
    test_disease_stage_distribution,
    test_n_animals_to_distribute
  )
  # Numbers in new row should sum to the number of animals to distribute
  expect_equal(sum(tail(test_result_duration_matrix)),
               test_n_animals_to_distribute)
  expected_duration_matrix <- matrix(c(rep(0, 8),
                                       # Values determined by random seed:
                                       0, 0, 6, 533, 4872, 12420, 17164, 15005),
                                     ncol = 8,
                                     byrow = TRUE)
  expect_equal(test_result_duration_matrix, expected_duration_matrix)
})

test_that("new disease stage row is appended correctly in repeated calls", {
  withr::local_seed(100)
  test_empty_duration_matrix <- matrix(0, ncol = 8)
  test_disease_stage_distribution <-
    c(0.000001, 0.000001, 0.0001, 0.01, 0.1, 0.25, 0.339898, 0.3)
  test_n_animals_to_distribute <- 50000

  # First call
  test_result_duration_matrix <- he_add_disease_stage_duration(
    test_empty_duration_matrix,
    test_disease_stage_distribution,
    test_n_animals_to_distribute
  )
  # Numbers in new row should sum to the number of animals to distribute
  expect_equal(sum(tail(test_result_duration_matrix)),
               test_n_animals_to_distribute)
  expected_duration_matrix <- matrix(c(rep(0, 8),
                                       # Values determined by random seed:
                                       0, 0, 6, 533, 4872, 12420, 17164, 15005),
                                     ncol = 8,
                                     byrow = TRUE)
  expect_equal(test_result_duration_matrix, expected_duration_matrix)

  # Second call
  test_result_duration_matrix_2 <- he_add_disease_stage_duration(
    test_result_duration_matrix,
    test_disease_stage_distribution,
    test_n_animals_to_distribute
  )
  # Numbers in new row should sum to the number of animals to distribute
  expect_equal(sum(tail(test_result_duration_matrix_2, n = 1)),
               test_n_animals_to_distribute)
  expected_duration_matrix <- matrix(c(rep(0, 8),
                                       # Values determined by random seed:
                                       0, 0, 6, 533, 4872, 12420, 17164, 15005,
                                       0, 0, 6, 495, 5078, 12477, 17054, 14890),
                                     ncol = 8,
                                     byrow = TRUE)
  expect_equal(test_result_duration_matrix_2, expected_duration_matrix)

  # Third call
  test_result_duration_matrix_3 <- he_add_disease_stage_duration(
    test_result_duration_matrix_2,
    test_disease_stage_distribution,
    0
  )
  # Numbers in new row should sum to the number of animals to distribute
  expect_equal(sum(tail(test_result_duration_matrix_3, n = 1)),
               0)
  expected_duration_matrix <- matrix(c(rep(0, 8),
                                       # Values determined by random seed:
                                       0, 0, 6, 533, 4872, 12420, 17164, 15005,
                                       0, 0, 6, 495, 5078, 12477, 17054, 14890,
                                       rep(0, 8)),
                                     ncol = 8,
                                     byrow = TRUE)
  expect_equal(test_result_duration_matrix_3, expected_duration_matrix)
})

# TODO: Check for an error if attempting to append to an empty duration matrix?
# Consider custom error message?
