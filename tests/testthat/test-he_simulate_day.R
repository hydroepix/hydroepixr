test_that("simulating a day with no remaining infections generates error", {
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_initialized.rds"
    ))
  test_species_info <- test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_net_pen_to_net_pen <- 0.5
  test_simulation_env <- rlang::new_environment()
  # Set up test output directory and file, initialize test environment
  temp_test_dir <-
    output_and_simulation_env_test_setup(
      test_simulation_env,
      test_species_info,
      test_net_pen_info,
      test_net_pen_to_net_pen,
      test_output_file_name = "infected_net_pens.csv"
    )

  expect_error(
    he_simulate_day(
      test_infected_net_pen_info,
      simulation_day = 1,
      test_simulation_env
    ),
    regexp = "No remaining infected net pens. Simulation should have terminated."
  )
})

test_that("day is simulated correctly for a single infected net pen", {
  withr::local_seed(100)
  test_infected_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/infected_net_pen_info_initialized.rds"
    ))
  test_species_info <-
    readRDS(paste0(
      test_data_filepath,
      "/species_info_guaranteed_infection.rds"
    ))
  test_net_pen_info <-
    readRDS(paste0(
      test_data_filepath,
      "/initialized_net_pen_info_guaranteed_infection.rds"
    ))
  test_net_pen_to_net_pen <- 0.5
  test_simulation_env <- rlang::new_environment()
  # Set up test output directory and file, initialize test environment
  temp_test_dir <-
    output_and_simulation_env_test_setup(
      test_simulation_env,
      test_species_info,
      test_net_pen_info,
      test_net_pen_to_net_pen,
      test_output_file_name = "infected_net_pens.csv"
    )

  test_n_index_infected_min <- 1
  test_n_index_infected_mode <- 10
  test_n_index_infected_max <- 100
  test_clinically_infected_prop <- 0.9

  test_index_net_pens <- 1
  test_index_infection_stage <- "subclinical-clinical split"
  test_infected_net_pen_info <-
    he_initialize_infection(
      test_infected_net_pen_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_net_pen_info,
      test_index_net_pens,
      test_index_infection_stage,
      test_clinically_infected_prop
    )

  # Simulate 10 days of infection dynamics ---
  # Day 1 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 1,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24971,
    n_latent = 15,
    n_subclinical = 4,
    n_clinical = 8,
    n_recovered = 1,
    n_dead = 1,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    test_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 1,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 0, 0, 3, 5, 4, 3),
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
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 2 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 2,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24959,
    n_latent = 27,
    n_subclinical = 1,
    n_clinical = 8,
    n_recovered = 4,
    n_dead = 1,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 2,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 0, 3, 6, 9, 6, 3),
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
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 3 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 3,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24949,
    n_latent = 37,
    n_subclinical = 0,
    n_clinical = 7,
    n_recovered = 5,
    n_dead = 2,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 3,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 3, 6, 10, 8, 7, 3),
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
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 4 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 4,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24943,
    n_latent = 43,
    n_subclinical = 0,
    n_clinical = 5,
    n_recovered = 5,
    n_dead = 4,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 4,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 3, 6, 10, 8, 10, 5, 1),
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
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 5 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 5,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24933,
    n_latent = 53,
    n_subclinical = 0,
    n_clinical = 4,
    n_recovered = 5,
    n_dead = 5,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 5,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(3, 6, 10, 8, 10, 8, 6, 2),
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
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 6 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 6,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24932,
    n_latent = 51,
    n_subclinical = 1,
    n_clinical = 4,
    n_recovered = 5,
    n_dead = 7,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 6,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(6, 10, 8, 10, 8, 6, 3, 0),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(1, 0, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(1, 2, 0, 1, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 7 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 7,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24928,
    n_latent = 49,
    n_subclinical = 2,
    n_clinical = 7,
    n_recovered = 6,
    n_dead = 8,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 7,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(10, 8, 10, 8, 6, 3, 3, 1),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(1, 1, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(2, 0, 2, 2, 0, 0, 1, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 8 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 8,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24918,
    n_latent = 49,
    n_subclinical = 5,
    n_clinical = 11,
    n_recovered = 7,
    n_dead = 10,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 8,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(8, 10, 8, 6, 5, 7, 2, 3),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(1, 3, 1, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(0, 3, 3, 0, 1, 2, 1, 1),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 9 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 9,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24899,
    n_latent = 60,
    n_subclinical = 7,
    n_clinical = 16,
    n_recovered = 8,
    n_dead = 10,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 9,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(10, 8, 6, 6, 8, 7, 8, 7),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(5, 1, 1, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(4, 3, 0, 2, 3, 1, 3, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )

  # Day 9 ---
  # Check infected net pen info
  expected_infected_net_pen_info <- data.frame(
    simulation_day = 10,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24874,
    n_latent = 75,
    n_subclinical = 6,
    n_clinical = 18,
    n_recovered = 13,
    n_dead = 14,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0,
    is_vaccinated = 0
  )
  result_infected_net_pen_info <- he_simulate_day(
    result_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 10,
    test_species_info
  )
  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
  # Check disease stage duration matrices
  expected_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(8, 6, 6, 8, 12, 15, 13, 7),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(1, 5, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(4, 0, 3, 5, 1, 4, 0, 1),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_disease_stage_duration_matrices
  )
})
