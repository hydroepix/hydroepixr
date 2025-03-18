test_that("invalid contact type produces error", {
  test_inf_farm_info <-
    readRDS(paste0(test_data_filepath, "/inf_farm_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  he_initialize_simulation_env(test_simulation_env, test_species_info)
  test_farm_info <-
    readRDS(paste0(test_data_filepath, "/initialized_farm_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_type_of_contact <- "immediate"
  expect_error(
    he_initialize_infection(
      test_inf_farm_info,
      test_simulation_env,
      test_species_info,
      test_farm_info,
      test_index_netpen_ids,
      test_type_of_contact
    ),
    regex = "Invalid type of contact for index farm infection."
  )
})

test_that("direct contact initializes infection correctly", {
  test_inf_farm_info <-
    readRDS(paste0(test_data_filepath, "/inf_farm_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  # TODO: Remove direct call to he_initialize_simulation_env and replace
  # with specific initialization steps?
  he_initialize_simulation_env(test_simulation_env, test_species_info)
  test_farm_info <-
    readRDS(paste0(test_data_filepath, "/initialized_farm_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_type_of_contact <- "direct"
  test_inf_farm_info <-
    he_initialize_infection(
      test_inf_farm_info,
      test_simulation_env,
      test_species_info,
      test_farm_info,
      test_index_netpen_ids,
      test_type_of_contact
    )
  # Check infected netpen added
  expect_equal(
    test_inf_farm_info,
    data.frame(
      netpen_id = 1,
      farm_id = 1,
      species_id = 1,
      within_netpen_transmission = 0.35105114,
      susceptible = 24999,
      latent = 0,
      subclinical = 1,
      clinical = 0,
      immune = 0,
      total = 25000,
      infection_status = 1,
      latent_duration = 0,
      subclinical_duration = 0,
      clinical_time = Inf,
      time_of_diagnosis = Inf,
      diagnosed = 0,
      infected_by_direct_contact = "direct",
      time_infected = 1,
      vaccinated = 0
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

test_that("indirect contact initializes infection correctly", {
  test_inf_farm_info <-
    readRDS(paste0(test_data_filepath, "/inf_farm_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  # TODO: Remove direct call to he_initialize_simulation_env and replace
  # with specific initialization steps?
  he_initialize_simulation_env(test_simulation_env, test_species_info)
  test_farm_info <-
    readRDS(paste0(test_data_filepath, "/initialized_farm_info_bay_x.rds"))
  test_index_netpen_ids <- 1
  test_type_of_contact <- "indirect"
  test_inf_farm_info <-
    he_initialize_infection(
      test_inf_farm_info,
      test_simulation_env,
      test_species_info,
      test_farm_info,
      test_index_netpen_ids,
      test_type_of_contact
    )
  # Check infected netpen added
  expect_equal(
    test_inf_farm_info,
    data.frame(
      netpen_id = 1,
      farm_id = 1,
      species_id = 1,
      within_netpen_transmission = 0.35105114,
      susceptible = 24999,
      latent = 1,
      subclinical = 0,
      clinical = 0,
      immune = 0,
      total = 25000,
      infection_status = 1,
      latent_duration = 0,
      subclinical_duration = 0,
      clinical_time = Inf,
      time_of_diagnosis = Inf,
      diagnosed = 0,
      infected_by_direct_contact = "indirect",
      time_infected = 1,
      vaccinated = 0
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
