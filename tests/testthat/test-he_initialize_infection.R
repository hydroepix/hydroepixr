test_that("invalid index disease stage produces error", {
  test_inf_farm_info <-
    readRDS(paste0(test_data_filepath, "/inf_farm_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")
  test_n_index_infected_min <- 1
  test_n_index_infected_mode <- 1
  test_n_index_infected_max <- 1

  test_farm_info <-
    readRDS(paste0(test_data_filepath, "/initialized_farm_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_index_infection_stage <- "invalid_disease_stage"
  expect_error(
    he_initialize_infection(
      test_inf_farm_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_farm_info,
      test_index_netpen_ids,
      test_index_infection_stage
    ),
    regex = "Invalid disease stage for index farm infection."
  )
})

test_that("default index infection initializes correctly", {
  test_inf_farm_info <-
    readRDS(paste0(test_data_filepath, "/inf_farm_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")
  test_n_index_infected_min <- 1
  test_n_index_infected_mode <- 1
  test_n_index_infected_max <- 1

  test_farm_info <-
    readRDS(paste0(test_data_filepath, "/initialized_farm_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_inf_farm_info <-
    he_initialize_infection(
      test_inf_farm_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_farm_info,
      test_index_netpen_ids
    )
  # Check infected netpen added
  expect_equal(
    test_inf_farm_info,
    data.frame(
      simulation_day = 0,
      netpen_id = 1,
      farm_id = 1,
      species_id = 1,
      within_netpen_transmission = 0.35105114,
      n_susceptible = 24999,
      n_latent = 0,
      n_subclinical = 1,
      n_clinical = 0,
      n_immune = 0,
      n_total = 25000,
      infection_status = 1,
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
    0)
})

test_that("specified disease stage for index infection initializes correctly", {
  test_inf_farm_info <-
    readRDS(paste0(test_data_filepath, "/inf_farm_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")
  test_n_index_infected_min <- 1
  test_n_index_infected_mode <- 1
  test_n_index_infected_max <- 1

  test_farm_info <-
    readRDS(paste0(test_data_filepath, "/initialized_farm_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_index_infection_stage <- "latent"
  test_inf_farm_info <-
    he_initialize_infection(
      test_inf_farm_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_farm_info,
      test_index_netpen_ids,
      test_index_infection_stage
    )
  # Check infected netpen added
  expect_equal(
    test_inf_farm_info,
    data.frame(
      simulation_day = 0,
      netpen_id = 1,
      farm_id = 1,
      species_id = 1,
      within_netpen_transmission = 0.35105114,
      n_susceptible = 24999,
      n_latent = 1,
      n_subclinical = 0,
      n_clinical = 0,
      n_immune = 0,
      n_total = 25000,
      infection_status = 1,
      infection_origin = "index",
      day_infected = 0,
      is_vaccinated = 0
    )
  )

  # Check disease stage durations have correct number of infected animals
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$latent_duration),
    1)
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$subclinical_duration),
    0)
  expect_equal(
    rowSums(test_simulation_env$disease_stage_duration_matrices$clinical_duration),
    0)
})
