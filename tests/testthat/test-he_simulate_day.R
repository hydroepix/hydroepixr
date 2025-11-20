test_that("simulating a day with no remaining infections generates error", {
  test_infected_netpen_info <-
    readRDS(paste0(test_data_filepath, "/infected_netpen_info_initialized.rds"))
  test_species_info <- test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  # Set up test output directory and file, initialize test environment
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")

  expect_error(
    he_simulate_day(test_infected_netpen_info, simulation_day = 1, test_simulation_env),
    regexp = "No remaining infected netpens. Simulation should have terminated."
  )
})

test_that("day is simulated correctly for a single infected netpen", {
  withr::local_seed(100)
  test_infected_netpen_info <-
    readRDS(paste0(test_data_filepath, "/infected_netpen_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/species_info_guaranteed_infection.rds"))
  test_netpen_info <-
    readRDS(paste0(test_data_filepath, "/initialized_netpen_info_guaranteed_infection.rds"))
  test_simulation_env <- rlang::new_environment()
  # Set up test output directory and file, initialize test environment
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")

  test_n_index_infected_min <- 1
  test_n_index_infected_mode <- 10
  test_n_index_infected_max <- 100
  test_clinically_infected_prop <- 0.9

  test_index_netpens <- 1
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
      test_index_netpens,
      test_index_infection_stage,
      test_clinically_infected_prop
    )

  # Simulate 10 days of infection dynamics ---
  # Day 1 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 1,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24975,
    n_latent = 11,
    n_subclinical = 4,
    n_clinical = 8,
    n_recovered = 1,
    n_dead = 1,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(test_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 1,
                                          test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 0, 0, 0, 5, 1, 5),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(3, 1, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(0, 1, 2, 1, 2, 1, 1, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 2 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 2,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24963,
    n_latent = 23,
    n_subclinical = 1,
    n_clinical = 8,
    n_recovered = 4,
    n_dead = 1,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 2,
                                          test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 0, 0, 8, 3, 9, 3),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(1, 0, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(1, 2, 1, 2, 1, 1, 0, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 3 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 3,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24951,
    n_latent = 35,
    n_subclinical = 0,
    n_clinical = 7,
    n_recovered = 5,
    n_dead = 2,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                                 test_simulation_env,
                                                 simulation_day = 3,
                                                 test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 0, 8, 3, 13, 9, 2),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(0, 0, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(2, 1, 2, 1, 1, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 4 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 4,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24944,
    n_latent = 42,
    n_subclinical = 0,
    n_clinical = 5,
    n_recovered = 5,
    n_dead = 4,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                                 test_simulation_env,
                                                 simulation_day = 4,
                                                 test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 8, 3, 13, 10, 6, 2),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(0, 0, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(1, 2, 1, 1, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 5 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 5,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24935,
    n_latent = 51,
    n_subclinical = 0,
    n_clinical = 4,
    n_recovered = 5,
    n_dead = 5,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                                 test_simulation_env,
                                                 simulation_day = 5,
                                                 test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 8, 3, 13, 10, 8, 4, 5),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(0, 0, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(2, 1, 1, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 6 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 6,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24931,
    n_latent = 55,
    n_subclinical = 0,
    n_clinical = 2,
    n_recovered = 5,
    n_dead = 7,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                                 test_simulation_env,
                                                 simulation_day = 6,
                                                 test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(8, 3, 13, 10, 9, 6, 5, 1),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(0, 0, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(1, 1, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 7 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 7,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24928,
    n_latent = 50,
    n_subclinical = 3,
    n_clinical = 6,
    n_recovered = 5,
    n_dead = 8,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                                 test_simulation_env,
                                                 simulation_day = 7,
                                                 test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(3, 13, 10, 9, 6, 6, 2, 1),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(2, 0, 1, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(1, 1, 1, 1, 0, 1, 1, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 8 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 8,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24917,
    n_latent = 58,
    n_subclinical = 2,
    n_clinical = 7,
    n_recovered = 7,
    n_dead = 9,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                                 test_simulation_env,
                                                 simulation_day = 8,
                                                 test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(13, 10, 9, 6, 8, 3, 5, 4),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(0, 2, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(1, 2, 1, 1, 1, 1, 0, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 9 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 9,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24908,
    n_latent = 54,
    n_subclinical = 7,
    n_clinical = 14,
    n_recovered = 7,
    n_dead = 10,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                                 test_simulation_env,
                                                 simulation_day = 9,
                                                 test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(10, 9, 6, 8, 4, 9, 7, 1),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(4, 2, 1, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(3, 2, 1, 2, 4, 1, 0, 1),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

  # Day 9 ---
  # Check infected netpen info
  expected_infected_netpen_info <- data.frame(
    simulation_day = 10,
    netpen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 1,
    n_susceptible = 24879,
    n_latent = 73,
    n_subclinical = 7,
    n_clinical = 17,
    n_recovered = 11,
    n_dead = 13,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                                 test_simulation_env,
                                                 simulation_day = 10,
                                                 test_species_info)
  expect_equal(result_infected_netpen_info, expected_infected_netpen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(9, 6, 8, 4, 12, 13, 13, 8),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(4, 2, 1, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(2, 3, 4, 4, 2, 1, 1, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(test_simulation_env$disease_stage_duration_matrices,
               expected_disease_stage_duration_matrices)

})
