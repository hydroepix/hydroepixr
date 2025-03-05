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

test_that("too few disease stage counts for the number of disease stage duration
          matrices generates an error", {

})

test_that("too many disease stage counts for the number of disease stage duration
          matrices generates an error", {

})

test_that("error generated when number of netpens recorded in disease stage
          counts does not match number of values provided for newly infected", {

})

test_that("error generated when number of netpens recorded in disease stage
          duration matrices does not match number of netpens recorded in disease
          stage duration matrices", {

})

test_that("error generated when number of netpens recorded in disease stage
          duration matrices does not match number of values provided for newly
          infected", {

})
