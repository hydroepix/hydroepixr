test_that("disease stage duration values are updated correctly", {
  withr::local_seed(100)
  test_duration_matrix <- matrix(c(0, 0, 1, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  test_disease_stage_distribution <-
    c(0.000001, 0.000001, 0.0001, 0.01, 0.1, 0.25, 0.339898, 0.3)
  test_num_animals_to_distribute <- 50000
  test_result_duration_matrix <-
    he_update_disease_stage_duration_matrix(
      test_disease_stage_distribution,
      test_duration_matrix,
      test_num_animals_to_distribute
    )
  # Numbers in updated row should sum to the number of animals to distribute
  # plus the number of animals in the starting duration
  expect_equal(sum(tail(test_result_duration_matrix)),
               sum(test_num_animals_to_distribute, test_duration_matrix))
  expected_duration_matrix <- matrix( # Values determined by random seed
    c(0, 1, 6, 533, 4872, 12420, 17164, 15005),
    ncol = 8,
    byrow = TRUE)
  expect_equal(test_result_duration_matrix, expected_duration_matrix)

})
