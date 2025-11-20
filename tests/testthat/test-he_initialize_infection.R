test_that("invalid index disease stage produces error", {
  test_infected_netpen_info <-
    readRDS(paste0(test_data_filepath, "/infected_netpen_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")
  test_n_index_infected_min <- 3
  test_n_index_infected_mode <- 3
  test_n_index_infected_max <- 3
  test_clinically_infected_prop <- 0.7

  test_netpen_info <-
    readRDS(paste0(test_data_filepath, "/initialized_netpen_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_index_infection_stage <- "invalid_disease_stage"
  expect_error(
    he_initialize_infection(
      test_infected_netpen_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_netpen_info,
      test_index_netpen_ids,
      test_index_infection_stage,
      test_clinically_infected_prop
    ),
    regex = "Invalid disease stage for index infection."
  )
})

test_that("lack of clinically_infected_prop argument in subclinical-clinical
          split mode generates error", {
  withr::local_seed(100)
  test_infected_netpen_info <-
    readRDS(paste0(test_data_filepath, "/infected_netpen_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")
  test_n_index_infected_min <- 3
  test_n_index_infected_mode <- 3
  test_n_index_infected_max <- 3

  test_netpen_info <-
    readRDS(paste0(test_data_filepath, "/initialized_netpen_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_index_infection_stage <- "subclinical-clinical split"
  expect_error(
    he_initialize_infection(
      test_infected_netpen_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_netpen_info,
      test_index_netpen_ids,
      test_index_infection_stage
      #no clinically_infected_prop provided
    ),
    regexp = "Error: clinically_infected_prop must be provided if infection is initialized as a subclinical-clinical infection split.")
})

test_that("subclinical-clinical split disease stage for index infection initializes correctly", {
  withr::local_seed(100)
  test_infected_netpen_info <-
    readRDS(paste0(test_data_filepath, "/infected_netpen_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")
  test_n_index_infected_min <- 3
  test_n_index_infected_mode <- 3
  test_n_index_infected_max <- 3
  test_clinically_infected_prop <- 0.7

  test_netpen_info <-
    readRDS(paste0(test_data_filepath, "/initialized_netpen_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_index_infection_stage <- "subclinical-clinical split"
  test_infected_netpen_info <-
    he_initialize_infection(
      test_infected_netpen_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_netpen_info,
      test_index_netpen_ids,
      test_index_infection_stage,
      test_clinically_infected_prop
    )
  # Check infected netpen added
  expect_equal(
    test_infected_netpen_info,
    data.frame(
      simulation_day = 0,
      netpen_id = 1,
      farm_id = 1,
      species_id = 1,
      within_netpen_transmission = 0.35105114,
      n_susceptible = 24997,
      n_latent = 0,
      n_subclinical = 1,
      n_clinical = 2,
      n_recovered = 0,
      n_dead = 0,
      n_total = 25000,
      infection_origin = "index",
      day_infected = 0,
      is_vaccinated = 0
    )
  )

  # Check disease stage durations have correct number of infected animals
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$latent_duration),
    0)
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$subclinical_duration),
    1)
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$clinical_duration),
    2)
})

test_that("latent disease stage for index infection initializes correctly", {
  withr::local_seed(100)
  test_infected_netpen_info <-
    readRDS(paste0(test_data_filepath, "/infected_netpen_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")
  test_n_index_infected_min <- 3
  test_n_index_infected_mode <- 3
  test_n_index_infected_max <- 3

  test_netpen_info <-
    readRDS(paste0(test_data_filepath, "/initialized_netpen_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_index_infection_stage <- "latent"
  test_infected_netpen_info <-
    he_initialize_infection(
      test_infected_netpen_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_netpen_info,
      test_index_netpen_ids,
      test_index_infection_stage
      # no clinically_infected_prop provided
    )
  # Check infected netpen added
  expect_equal(
    test_infected_netpen_info,
    data.frame(
      simulation_day = 0,
      netpen_id = 1,
      farm_id = 1,
      species_id = 1,
      within_netpen_transmission = 0.35105114,
      n_susceptible = 24997,
      n_latent = 3,
      n_subclinical = 0,
      n_clinical = 0,
      n_recovered = 0,
      n_dead = 0,
      n_total = 25000,
      infection_origin = "index",
      day_infected = 0,
      is_vaccinated = 0
    )
  )

  # Check disease stage durations have correct number of infected animals
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$latent_duration),
    3)
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$subclinical_duration),
    0)
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$clinical_duration),
    0)
})
