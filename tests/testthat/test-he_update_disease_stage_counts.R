test_that("disease stage count values are updated correctly for a single netpen", {
  test_disease_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- 1000
  test_case_fatality_prop <- 0.5
  test_updated_disease_stage_counts <-
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_n_newly_infected,
                                   test_case_fatality_prop)
  expected_updated_disease_stage_counts <-
    data.frame(
      n_susceptible = 25000 - 1000,
      n_latent = 10000 + 1000 - 25,
      n_subclinical = 10000 + 25 - 10,
      n_clinical = 5000 + 10 - 5,
      n_recovered = 0 + 2,
      n_dead = 0 + 3
    )
  expect_equal(test_updated_disease_stage_counts,
               expected_updated_disease_stage_counts)
})

test_that("disease stage count values are updated correctly for multiple netpens", {
  test_disease_stage_counts <- data.frame(
    n_susceptible = c(25000, 10000),
    n_latent = c(10000, 5000),
    n_subclinical = c(10000, 2500),
    n_clinical = c(5000, 1000),
    n_recovered = c(500, 0),
    n_dead = c(0, 0)
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
  test_n_newly_infected <- c(2000, 1000)
  test_case_fatality_prop <- 0.5
  test_updated_disease_stage_counts <-
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_n_newly_infected,
                                   test_case_fatality_prop)
  expected_updated_disease_stage_counts <-
    test_disease_stage_counts <- data.frame(
      n_susceptible = c(25000 - 2000,
                      10000 - 1000),
      n_latent = c(10000 + 2000 - 50,
                 5000 + 1000 - 25),
      n_subclinical = c(10000 + 50 - 25,
                      2500 + 25 - 10),
      n_clinical = c(5000 + 25 - 10,
                   1000 + 10 - 5),
      n_recovered = c(500 + 5,
                      0 + 2),
      n_dead = c(0 + 5,
                 0 + 3)
    )
  expect_equal(test_updated_disease_stage_counts,
               expected_updated_disease_stage_counts)
})

test_that("disease stage count values are sequentially updated correctly for multiple netpens ", {
  test_disease_stage_counts <- data.frame(
    n_susceptible = c(25000, 10000),
    n_latent = c(10000, 5000),
    n_subclinical = c(10000, 2500),
    n_clinical = c(5000, 1000),
    n_recovered = c(500, 0),
    n_dead = c(0, 0)
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
  test_n_newly_infected <- c(2000, 1000)
  test_case_fatality_prop <- 0.5
  # FIRST DISEASE STAGE COUNT UPDATE!!
  test_updated_disease_stage_counts <-
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_n_newly_infected,
                                   test_case_fatality_prop)
  # SECOND DISEASE STAGE COUNT UPDATE!!
  test_updated_disease_stage_counts <-
    he_update_disease_stage_counts(test_updated_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_n_newly_infected,
                                   test_case_fatality_prop)
  expected_updated_disease_stage_counts <-
    test_disease_stage_counts <- data.frame(
      n_susceptible = c(25000 - 4000,
                      10000 - 2000),
      n_latent = c(10000 + 4000 - 100,
                 5000 + 2000 - 50),
      n_subclinical = c(10000 + 100 - 50,
                      2500 + 50 - 20),
      n_clinical = c(5000 + 50 - 20,
                   1000 + 20 - 10),
      n_recovered = c(500 + 10,
                      0 + 4),
      n_dead = c(10, 6)
    )
  expect_equal(test_updated_disease_stage_counts,
               expected_updated_disease_stage_counts)
})

test_that("providing fewer than 4 columns for disease stage counts generates an
          error", {
  test_disease_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_recovered = 10000,
    n_dead = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- 1000
  test_case_fatality_prop <- 0.5
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_num_newly_infected,
                                   test_case_fatality_prop),
    regexp = "At least four disease stage counts are required in order to
         calculate transition between a beginning stage, an intermediate stage,
         and two final stages of disease. Please include at least four disease
         stage count columns."
    )
})

test_that("error generated when number of netpens recorded in disease stage
          counts does not match number of values provided for newly infected", {
  test_disease_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_immune = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- c(1000, 1000)
  test_case_fatality_prop <- 0.5
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_n_newly_infected,
                                   test_case_fatality_prop),
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
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- c(1000, 1000)
  test_case_fatality_prop <- 0.5
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_n_newly_infected,
                                   test_case_fatality_prop),
    regexp = "Inconsistent number of rows in disease stage duration matrices.
         Different matrices have numbers of rows as follows: 2, 1, 1",
    fixed = TRUE
  )
})

test_that("error generated when number of netpens recorded in disease stage
          counts does not match number of netpens recorded in disease
          stage duration matrices", {
  test_disease_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
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
  test_n_newly_infected <- 1000
  test_case_fatality_prop <- 0.5
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_n_newly_infected,
                                   test_case_fatality_prop),
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
      n_susceptible = 25000,
      n_latent = 10000,
      n_clinical = 5000,
      n_recovered = 0,
      n_dead = 0
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
    test_n_newly_infected <- 1000
    test_case_fatality_prop <- 0.5
    expect_error(
      he_update_disease_stage_counts(test_disease_stage_counts,
                                     test_disease_stage_duration_matrices,
                                     test_n_newly_infected,
                                     test_case_fatality_prop),
      regexp = "Mismatched number of disease stage counts and disease stage duration
         matrices. The number of disease stage duration matrices should be three
         less than the number of disease stage counts. \n
         Number of disease stage duration matrices: 3\n
         Number of disease stage counts: 5",
      fixed = TRUE
    )
})

test_that("too many disease stage counts for the number of disease stage duration
          matrices generates an error", {
  test_disease_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
  )
  test_disease_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0),
           ncol = 8,
           byrow = TRUE)
  )
  test_n_newly_infected <- 1000
  test_case_fatality_prop <- 0.5
  expect_error(
    he_update_disease_stage_counts(test_disease_stage_counts,
                                   test_disease_stage_duration_matrices,
                                   test_n_newly_infected,
                                   test_case_fatality_prop),
    regexp = "Mismatched number of disease stage counts and disease stage duration
         matrices. The number of disease stage duration matrices should be three
         less than the number of disease stage counts. \n
         Number of disease stage duration matrices: 2\n
         Number of disease stage counts: 6",
    fixed = TRUE
  )
})
