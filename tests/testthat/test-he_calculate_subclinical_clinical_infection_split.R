test_that(
  "subclinical-clinical split is calculated correctly with no animals
  transitioning between disease stages",
  {
    test_n_transitioning <- 0
    test_clinically_infected_prop <- 0.9
    test_subclinical_clinical_infection_split <-
      he_calculate_subclinical_clinical_infection_split(
        test_n_transitioning,
        test_clinically_infected_prop
      )
    expected_subclinical_clinical_infection_split <- cbind(0, 0)
    expect_equal(test_subclinical_clinical_infection_split,
                 expected_subclinical_clinical_infection_split)
  }
)

test_that(
  "subclinical-clinical split is calculated correctly with no clinical
  infection",
  {
    test_n_transitioning <- 1000
    test_clinically_infected_prop <- 0
    test_subclinical_clinical_infection_split <-
      he_calculate_subclinical_clinical_infection_split(
        test_n_transitioning,
        test_clinically_infected_prop
      )
    expected_subclinical_clinical_infection_split <- cbind(1000, 0)
    expect_equal(test_subclinical_clinical_infection_split,
                 expected_subclinical_clinical_infection_split)
  }
)

test_that(
  "subclinical-clinical split is calculated correctly with guaranteed clinical
  infection",
  {
    test_n_transitioning <- 1000
    test_clinically_infected_prop <- 1
    test_subclinical_clinical_infection_split <-
      he_calculate_subclinical_clinical_infection_split(
        test_n_transitioning,
        test_clinically_infected_prop
      )
    expected_subclinical_clinical_infection_split <- cbind(0, 1000)
    expect_equal(test_subclinical_clinical_infection_split,
                 expected_subclinical_clinical_infection_split)
  }
)

test_that(
  "subclinical-clinical split is calculated correctly with clinical infection
  for a single row",
  {
    test_n_transitioning <- 1000
    test_clinically_infected_prop <- 0.5
    test_subclinical_clinical_infection_split <-
      he_calculate_subclinical_clinical_infection_split(
        test_n_transitioning,
        test_clinically_infected_prop
      )
    expected_subclinical_clinical_infection_split <- cbind(500, 500)
    expect_equal(test_subclinical_clinical_infection_split,
                 expected_subclinical_clinical_infection_split)
  }
)

test_that(
  "subclinical-clinical split is calculated correctly with clinical infection
  for multiple rows",
  {
    test_n_transitioning <- c(2000, 1000)
    test_clinically_infected_prop <- 0.5
    test_subclinical_clinical_infection_split <-
      he_calculate_subclinical_clinical_infection_split(
        test_n_transitioning,
        test_clinically_infected_prop
      )
    expected_subclinical_clinical_infection_split <- rbind(cbind(1000, 1000),
                                                           cbind(500, 500))
    expect_equal(test_subclinical_clinical_infection_split,
                 expected_subclinical_clinical_infection_split)
  }
)
