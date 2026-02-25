test_that("disease stage duration values are updated correctly", {
  withr::local_seed(100)
  test_duration_matrix <-
    list(latent_duration = matrix(c(0, 0, 1, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE))
  test_disease_stage_distribution <-
    list(latent_dur_freq = list(c(0.000001, 0.000001, 0.0001, 0.01, 0.1, 0.25, 0.339898, 0.3)))
  test_n_animals_to_distribute <- data.frame(n_latent_in = 50000)
  test_result_duration_matrix <-
    he_update_disease_stage_duration_matrix(
      test_duration_matrix,
      test_disease_stage_distribution,
      test_n_animals_to_distribute
    )
  # Numbers in updated row should sum to the number of animals to distribute
  # plus the number of animals in the starting duration
  # expect_equal(sum(tail(test_result_duration_matrix)),
  #              sum(test_n_animals_to_distribute, test_duration_matrix))
  expected_duration_matrix <- list(latent_duration =
                                     # Values determined by random seed
                                     matrix(
                                       c(0, 1, 6, 533, 4872, 12420, 17164, 15005),
                                       ncol = 8,
                                       byrow = TRUE
                                     ))
  expect_equal(test_result_duration_matrix, expected_duration_matrix)

})

test_that("disease stage duration values are updated correctly for multiple disease stages", {
  withr::local_seed(100)
  test_duration_matrix <-
    list(
      latent_duration = matrix(c(0, 0, 1, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
      subclinical_duration = matrix(c(0, 0, 1, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
    )
  test_disease_stage_distribution <-
    list(
      latent_dur_freq = list(c(0.000001, 0.000001, 0.0001, 0.01, 0.1, 0.25, 0.339898, 0.3)),
      subclinical_dur_freq = list(c(0.249995, 0.50, 0.25, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001))
    )
  test_n_animals_to_distribute <- data.frame(n_latent_in = 50000,
                                             n_subclinical_in = 25000)
  test_result_duration_matrix <-
    he_update_disease_stage_duration_matrix(
      test_duration_matrix,
      test_disease_stage_distribution,
      test_n_animals_to_distribute
    )
  # Numbers in updated row should sum to the number of animals to distribute
  # plus the number of animals in the starting duration
  # expect_equal(sum(tail(test_result_duration_matrix)),
  #              sum(test_n_animals_to_distribute, test_duration_matrix))
  expected_duration_matrix <- list(latent_duration =
                                     # Values determined by random seed
                                     matrix(
                                       c(0, 1, 6, 533, 4872, 12420, 17164, 15005),
                                       ncol = 8,
                                       byrow = TRUE
                                     ),
                                   subclinical_duration =
                                     # Values determined by random seed
                                     matrix(
                                       c(6253, 12549, 6199, 0, 0, 0, 0, 0),
                                       ncol = 8,
                                       byrow = TRUE
                                     ))
  expect_equal(test_result_duration_matrix, expected_duration_matrix)

})


test_that("disease stage duration values are updated correctly for multiple disease stages and multiple infected net pens", {
  withr::local_seed(100)
  test_duration_matrix <-
    list(
      latent_duration = matrix(c(0, 0, 1, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
      subclinical_duration = matrix(c(0, 0, 1, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
    )
  test_disease_stage_distribution <-
    list(
      latent_dur_freq = list(c(0.000001, 0.000001, 0.0001, 0.01, 0.1, 0.25, 0.339898, 0.3)),
      subclinical_dur_freq = list(c(0.249995, 0.50, 0.25, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001))
    )
  test_n_animals_to_distribute <- data.frame(n_latent_in = c(50000, 25000),
                                             n_subclinical_in = c(25000, 10000))
  test_result_duration_matrix <-
    he_update_disease_stage_duration_matrix(
      test_duration_matrix,
      test_disease_stage_distribution,
      test_n_animals_to_distribute
    )
  # Numbers in updated row should sum to the number of animals to distribute
  # plus the number of animals in the starting duration
  # expect_equal(sum(tail(test_result_duration_matrix)),
  #              sum(test_n_animals_to_distribute, test_duration_matrix))
  expected_duration_matrix <- list(latent_duration =
                                     # Values determined by random seed
                                     matrix(
                                       c(0, 1, 6, 533, 4872, 12420, 17164, 15005,
                                         0, 0, 6, 251, 2486, 6251, 8599, 7407),
                                       ncol = 8,
                                       byrow = TRUE
                                     ),
                                   subclinical_duration =
                                     # Values determined by random seed
                                     matrix(
                                       c(6365, 12559, 6077, 0, 0, 0, 0, 0,
                                         2461, 5069, 2470, 0, 0, 0, 0, 0),
                                       ncol = 8,
                                       byrow = TRUE
                                     ))
  expect_equal(test_result_duration_matrix, expected_duration_matrix)

})
