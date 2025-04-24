test_that("disease stage count values are updated correctly for a single netpen", {
  test_disease_stage_counts <- data.frame(
    susceptible = 25000,
    latent = 10000,
    subclinical = 10000,
    clinical = 5000,
    immune = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_num_newly_infected <- 1000
  test_updated_disease_stage_counts <-
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected)
  expected_updated_disease_stage_counts <-
    data.frame(
      susceptible = 25000 - 1000,
      latent = 10000 + 1000 - 25,
      subclinical = 10000 + 25 - 10,
      clinical = 5000 + 10 - 5,
      immune = 0 + 5
    )
  expect_equal(test_updated_disease_stage_counts,
               expected_updated_disease_stage_counts)
})

test_that("disease stage count values are updated correctly for multiple netpens", {
  test_disease_stage_counts <- data.frame(
    susceptible = c(25000, 10000),
    latent = c(10000, 5000),
    subclinical = c(10000, 2500),
    clinical = c(5000, 1000),
    immune = c(500, 0)
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0,
             25, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0,
             10, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0,
             5, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE)
  )
  test_num_newly_infected <- c(2000, 1000)
  test_updated_disease_stage_counts <-
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected)
  expected_updated_disease_stage_counts <-
    test_disease_stage_counts <- data.frame(
      susceptible = c(25000 - 2000,
                      10000 - 1000),
      latent = c(10000 + 2000 - 50,
                 5000 + 1000 - 25),
      subclinical = c(10000 + 50 - 25,
                      2500 + 25 - 10),
      clinical = c(5000 + 25 - 10,
                   1000 + 10 - 5),
      immune = c(500 + 10,
                 0 + 5)
    )
  expect_equal(test_updated_disease_stage_counts,
               expected_updated_disease_stage_counts)
})

# TODO: Check subsequent updates
test_that("disease stage count values are sequentially updated correctly for multiple netpens", {
  test_disease_stage_counts <- data.frame(
    susceptible = c(25000, 10000),
    latent = c(10000, 5000),
    subclinical = c(10000, 2500),
    clinical = c(5000, 1000),
    immune = c(500, 0)
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0,
             25, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0,
             10, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0,
             5, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE)
  )
  test_num_newly_infected <- c(2000, 1000)
  test_updated_disease_stage_counts <-
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected)
  # Update a second time
  test_updated_disease_stage_counts <-
    he_update_disease_stage_counts(test_updated_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected)
  expected_updated_disease_stage_counts <-
    test_disease_stage_counts <- data.frame(
      susceptible = c(25000 - 4000,
                      10000 - 2000),
      latent = c(10000 + 4000 - 100,
                 5000 + 2000 - 50),
      subclinical = c(10000 + 100 - 50,
                      2500 + 50 - 20),
      clinical = c(5000 + 50 - 20,
                   1000 + 20 - 10),
      immune = c(500 + 20,
                 0 + 10)
    )
  expect_equal(test_updated_disease_stage_counts,
               expected_updated_disease_stage_counts)
})

test_that("providing fewer than 3 columns for disease stage counts generates an
          error", {
  test_disease_stage_counts <- data.frame(
    susceptible = 25000,
    immune = 10000
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_num_newly_infected <- 1000
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected),
    regexp = "At least three disease stage counts are required in order to
         calculate transition between a beginning, intermediate, and final stage
         of disease. Please include at least three disease stage count columns."
    )
})

test_that("error generated when number of netpens recorded in disease stage
          counts does not match number of values provided for newly infected", {
  test_disease_stage_counts <- data.frame(
    susceptible = 25000,
    latent = 10000,
    subclinical = 10000,
    clinical = 5000,
    immune = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_num_newly_infected <- c(1000, 1000)
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected),
    regexp = "Mismatched number of infected netpens (rows) between disease stage
         counts and number of newly infected animals. The number of newly
         infected animal values should be equal to the number of rows of
         disease stage counts.\n
         Number of rows in disease stage counts: 1\n
         Number of newly infected animal values: 2",
    fixed = TRUE
  )
})

test_that("error generated when number of netpens recorded in disease stage
          duration matrices is not consistent across matrices", {
  test_disease_stage_counts <- data.frame(
    susceptible = 25000,
    latent = 10000,
    subclinical = 10000,
    clinical = 5000,
    immune = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_num_newly_infected <- c(1000, 1000)
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected),
    regexp = "Inconsistent number of rows in disease stage duration matrices.
         Different matrices have numbers of rows as follows: 2, 1, 1",
    fixed = TRUE
  )
})

test_that("error generated when number of netpens recorded in disease stage
          counts does not match number of netpens recorded in disease
          stage duration matrices", {
  test_disease_stage_counts <- data.frame(
    susceptible = 25000,
    latent = 10000,
    subclinical = 10000,
    clinical = 5000,
    immune = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE)
  )
  test_num_newly_infected <- 1000
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected),
    regexp = "Mismatched number of infected netpens (rows) between disease stage
         counts and disease stage duration matrices. All duration matrices and
         the disease stage counts should all have the same number of rows.\n
         Number of rows in each disease stage duration matrix: 2, 2, 2\n
         Number of rows in disease stage counts: 1",
    fixed = TRUE
  )
})

test_that("too few disease stage counts for the number of disease stage duration
          matrices generates an error", {
    test_disease_stage_counts <- data.frame(
      susceptible = 25000,
      latent = 10000,
      clinical = 5000,
      immune = 0
    )
    test_disease_stage_duration_matrices <- list(
      matrix(c(50, 0, 0, 0, 0, 0, 0, 0),
             ncol = 8,
             byrow = TRUE),
      matrix(c(25, 0, 0, 0, 0, 0, 0, 0),
             ncol = 8,
             byrow = TRUE),
      matrix(c(10, 0, 0, 0, 0, 0, 0, 0),
             ncol = 8,
             byrow = TRUE)
    )
    test_num_newly_infected <- 1000
    expect_error(
      he_update_disease_stage_counts(test_disease_stage_counts,
                                     test_disease_stage_duration_matrices,
                                     test_num_newly_infected),
      regexp = "Mismatched number of disease stage counts and disease stage duration
         matrices. The number of disease stage duration matrices should be two
         less than the number of disease stage counts. \n
         Number of disease stage duration matrices: 3\n
         Number of disease stage counts: 4",
      fixed = TRUE
    )
})

test_that("too many disease stage counts for the number of disease stage duration
          matrices generates an error", {
  test_disease_stage_counts <- data.frame(
    susceptible = 25000,
    latent = 10000,
    subclinical = 10000,
    clinical = 5000,
    immune = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE)
  )
  test_num_newly_infected <- 1000
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected),
    regexp = "Mismatched number of disease stage counts and disease stage duration
         matrices. The number of disease stage duration matrices should be two
         less than the number of disease stage counts. \n
         Number of disease stage duration matrices: 2\n
         Number of disease stage counts: 5",
    fixed = TRUE
  )
})
