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
    expected_subclinical_clinical_infection_split <- c(0, 0)
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
    expected_subclinical_clinical_infection_split <- c(1000, 0)
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
    expected_subclinical_clinical_infection_split <- c(0, 1000)
    expect_equal(test_subclinical_clinical_infection_split,
                 expected_subclinical_clinical_infection_split)
  }
)

test_that(
  "subclinical-clinical split is calculated correctly with clinical infection",
  {
    test_n_transitioning <- 1000
    test_clinically_infected_prop <- 0.5
    test_subclinical_clinical_infection_split <-
      he_calculate_subclinical_clinical_infection_split(
        test_n_transitioning,
        test_clinically_infected_prop
      )
    expected_subclinical_clinical_infection_split <- c(500, 500)
    expect_equal(test_subclinical_clinical_infection_split,
                 expected_subclinical_clinical_infection_split)
  }
)
