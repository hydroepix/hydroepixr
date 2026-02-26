test_that("infection stage count values are updated correctly for a single
          net pen", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
  )
  test_infection_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE), # latent
    matrix(c(20, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE), # subclinical
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE) # clinical
  )
  test_n_newly_infected <- 1000
  test_clinically_infected_prop <- 0.5
  test_updated_infection_stage_counts <-
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  expected_updated_infection_stage_counts <-
    data.frame(
      n_susceptible = 25000 - 1000,
      n_latent = 10000 + 1000 - 50,
      n_subclinical = 10000 + 25 - 20,
      n_clinical = 5000 + 25 - 10,
      n_recovered = 0 + 20,
      n_dead = 0 + 10
    )
  expect_equal(
    test_updated_infection_stage_counts,
    expected_updated_infection_stage_counts
  )
})

test_that("infection stage count values are updated correctly for multiple
          net pens", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = c(25000, 10000),
    n_latent = c(10000, 5000),
    n_subclinical = c(10000, 2500),
    n_clinical = c(5000, 1000),
    n_recovered = c(500, 0),
    n_dead = c(0, 0)
  )
  test_infection_stage_duration_matrices <- list(
    matrix(
      c(50, 0, 0, 0, 0, 0, 0, 0, 40, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    matrix(
      c(30, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    matrix(
      c(30, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    )
  )
  test_n_newly_infected <- c(2000, 1000)
  test_clinically_infected_prop <- 0.5
  test_updated_infection_stage_counts <-
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  expected_updated_infection_stage_counts <-
    test_infection_stage_counts <- data.frame(
      n_susceptible = c(25000 - 2000, 10000 - 1000),
      n_latent = c(10000 + 2000 - 50, 5000 + 1000 - 40),
      n_subclinical = c(10000 + 25 - 30, 2500 + 20 - 10),
      n_clinical = c(5000 + 25 - 30, 1000 + 20 - 5),
      n_recovered = c(500 + 30, 0 + 10),
      n_dead = c(0 + 30, 0 + 5)
    )
  expect_equal(
    test_updated_infection_stage_counts,
    expected_updated_infection_stage_counts
  )
})

test_that("infection stage count values are sequentially updated correctly for
          multiple net pens ", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = c(25000, 10000),
    n_latent = c(10000, 5000),
    n_subclinical = c(10000, 2500),
    n_clinical = c(5000, 1000),
    n_recovered = c(500, 0),
    n_dead = c(0, 0)
  )
  test_infection_stage_duration_matrices <- list(
    matrix(
      c(100, 0, 0, 0, 0, 0, 0, 0, 50, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    matrix(
      c(25, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    matrix(
      c(10, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    )
  )
  test_n_newly_infected <- c(2000, 1000)
  test_clinically_infected_prop <- 0.5
  # FIRST infection STAGE COUNT UPDATE!!
  test_updated_infection_stage_counts <-
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  # SECOND infection STAGE COUNT UPDATE!!
  test_updated_infection_stage_counts <-
    he_update_infection_stage_counts(
      test_updated_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    )
  expected_updated_infection_stage_counts <-
    test_infection_stage_counts <- data.frame(
      n_susceptible = c(25000 - 4000, 10000 - 2000),
      n_latent = c(10000 + 4000 - 200, 5000 + 2000 - 100),
      n_subclinical = c(10000 + 100 - 50, 2500 + 50 - 20),
      n_clinical = c(5000 + 100 - 20, 1000 + 50 - 10),
      n_recovered = c(500 + 50, 0 + 20),
      n_dead = c(20, 10)
    )
  expect_equal(
    test_updated_infection_stage_counts,
    expected_updated_infection_stage_counts
  )
})

test_that("providing fewer than 4 columns for infection stage counts generates an
          error", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_recovered = 10000,
    n_dead = 0
  )
  test_infection_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- 1000
  test_clinically_infected_prop <- 0.5
  expect_error(
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_num_newly_infected,
      test_clinically_infected_prop
    ),
    regexp = "At least four infection stage counts are required in order to
         calculate transition between a beginning stage, an intermediate stage,
         and two final stages of infection. Please include at least four infection
         stage count columns."
  )
})

test_that("error generated when number of net pens recorded in infection stage
          counts does not match number of values provided for newly infected", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_immune = 0
  )
  test_infection_stage_duration_matrices <- list(
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- c(1000, 1000)
  test_clinically_infected_prop <- 0.5
  expect_error(
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    ),
    regexp = "Mismatched number of infected net pens (rows) between infection stage
         counts and number of newly infected animals. The number of newly
         infected animal values should be equal to the number of rows of
         infection stage counts.\n
         Number of rows in infection stage counts: 1\n
         Number of newly infected animal values: 2",
    fixed = TRUE
  )
})

test_that("error generated when number of net pens recorded in infection stage
          duration matrices is not consistent across matrices", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
  )
  test_infection_stage_duration_matrices <- list(
    matrix(
      c(25, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(5, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- c(1000, 1000)
  test_clinically_infected_prop <- 0.5
  expect_error(
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    ),
    regexp = "Inconsistent number of rows in infection stage duration matrices.
         Different matrices have numbers of rows as follows: 2, 1, 1",
    fixed = TRUE
  )
})

test_that("error generated when number of net pens recorded in infection stage
          counts does not match number of net pens recorded in infection
          stage duration matrices", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
  )
  test_infection_stage_duration_matrices <- list(
    matrix(
      c(50, 0, 0, 0, 0, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    matrix(
      c(25, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    ),
    matrix(
      c(10, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0),
      ncol = 8,
      byrow = TRUE
    )
  )
  test_n_newly_infected <- 1000
  test_clinically_infected_prop <- 0.5
  expect_error(
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    ),
    regexp = "Mismatched number of infected net pens (rows) between infection stage
         counts and infection stage duration matrices. All duration matrices and
         the infection stage counts should all have the same number of rows.\n
         Number of rows in each infection stage duration matrix: 2, 2, 2\n
         Number of rows in infection stage counts: 1",
    fixed = TRUE
  )
})

test_that("too few infection stage counts for the number of infection stage duration
          matrices generates an error", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
  )
  test_infection_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(10, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- 1000
  test_clinically_infected_prop <- 0.5
  expect_error(
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    ),
    regexp = "Mismatched number of infection stage counts and infection stage duration
         matrices. The number of infection stage duration matrices should be three
         less than the number of infection stage counts. \n
         Number of infection stage duration matrices: 3\n
         Number of infection stage counts: 5",
    fixed = TRUE
  )
})

test_that("too many infection stage counts for the number of infection stage duration
          matrices generates an error", {
  test_infection_stage_counts <- data.frame(
    n_susceptible = 25000,
    n_latent = 10000,
    n_subclinical = 10000,
    n_clinical = 5000,
    n_recovered = 0,
    n_dead = 0
  )
  test_infection_stage_duration_matrices <- list(
    matrix(c(50, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE),
    matrix(c(25, 0, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
  )
  test_n_newly_infected <- 1000
  test_clinically_infected_prop <- 0.5
  expect_error(
    he_update_infection_stage_counts(
      test_infection_stage_counts,
      test_infection_stage_duration_matrices,
      test_n_newly_infected,
      test_clinically_infected_prop
    ),
    regexp = "Mismatched number of infection stage counts and infection stage duration
         matrices. The number of infection stage duration matrices should be three
         less than the number of infection stage counts. \n
         Number of infection stage duration matrices: 2\n
         Number of infection stage counts: 6",
    fixed = TRUE
  )
})
