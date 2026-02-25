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
  expected_day_1_infected_net_pen_info <- data.frame(
    simulation_day = 1,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24967,
    n_latent = 19,
    n_subclinical = 4,
    n_clinical = 8,
    n_recovered = 1,
    n_dead = 1,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_1_infected_net_pen_info <- he_simulate_day(
    test_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 1,
    test_species_info
  )
  expect_equal(
    day_1_infected_net_pen_info,
    expected_day_1_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_1_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 0, 0, 1, 5, 9, 4),
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
    expected_day_1_disease_stage_duration_matrices
  )

  # Day 2 ---
  # Check infected net pen info
  expected_day_2_infected_net_pen_info <- data.frame(
    simulation_day = 2,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24954,
    n_latent = 32,
    n_subclinical = 1,
    n_clinical = 8,
    n_recovered = 4,
    n_dead = 1,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_2_infected_net_pen_info <- he_simulate_day(
    day_1_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 2,
    test_species_info
  )
  expect_equal(
    day_2_infected_net_pen_info,
    expected_day_2_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_2_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 0, 1, 7, 13, 7, 4),
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
    expected_day_2_disease_stage_duration_matrices
  )

  # Day 3 ---
  # Check infected net pen info
  expected_day_3_infected_net_pen_info <- data.frame(
    simulation_day = 3,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24944,
    n_latent = 42,
    n_subclinical = 0,
    n_clinical = 7,
    n_recovered = 5,
    n_dead = 2,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_3_infected_net_pen_info <- he_simulate_day(
    day_2_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 3,
    test_species_info
  )
  expect_equal(
    day_3_infected_net_pen_info,
    expected_day_3_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_3_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 0, 1, 7, 15, 10, 8, 1),
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
    expected_day_3_disease_stage_duration_matrices
  )

  # Day 4 ---
  # Check infected net pen info
  expected_day_4_infected_net_pen_info <- data.frame(
    simulation_day = 4,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24941,
    n_latent = 45,
    n_subclinical = 0,
    n_clinical = 5,
    n_recovered = 5,
    n_dead = 4,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_4_infected_net_pen_info <- he_simulate_day(
    day_3_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 4,
    test_species_info
  )
  expect_equal(
    day_4_infected_net_pen_info,
    expected_day_4_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_4_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(0, 1, 7, 15, 10, 9, 1, 2),
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
    expected_day_4_disease_stage_duration_matrices
  )

  # Day 5 ---
  # Check infected net pen info
  expected_day_5_infected_net_pen_info <- data.frame(
    simulation_day = 5,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24934,
    n_latent = 52,
    n_subclinical = 0,
    n_clinical = 4,
    n_recovered = 5,
    n_dead = 5,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_5_infected_net_pen_info <- he_simulate_day(
    day_4_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 5,
    test_species_info
  )
  expect_equal(
    day_5_infected_net_pen_info,
    expected_day_5_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_5_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(1, 7, 15, 10, 9, 2, 6, 2),
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
    expected_day_5_disease_stage_duration_matrices
  )

  # Day 6 ---
  # Check infected net pen info
  expected_day_6_infected_net_pen_info <- data.frame(
    simulation_day = 6,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24932,
    n_latent = 53,
    n_subclinical = 0,
    n_clinical = 3,
    n_recovered = 5,
    n_dead = 7,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_6_infected_net_pen_info <- he_simulate_day(
    day_5_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 6,
    test_species_info
  )
  expect_equal(
    day_6_infected_net_pen_info,
    expected_day_6_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_6_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(7, 15, 10, 9, 2, 7, 3, 0),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(0, 0, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(2, 1, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_6_disease_stage_duration_matrices
  )

  # Day 7 ---
  # Check infected net pen info
  expected_day_7_infected_net_pen_info <- data.frame(
    simulation_day = 7,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24930,
    n_latent = 48,
    n_subclinical = 2,
    n_clinical = 6,
    n_recovered = 5,
    n_dead = 9,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_7_infected_net_pen_info <- he_simulate_day(
    day_6_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 7,
    test_species_info
  )
  expect_equal(
    day_7_infected_net_pen_info,
    expected_day_7_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_7_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(15, 10, 9, 2, 7, 4, 1, 0),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(0, 2, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(2, 1, 0, 0, 1, 0, 2, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_7_disease_stage_duration_matrices
  )

  # Day 8 ---
  # Check infected net pen info
  expected_day_8_infected_net_pen_info <- data.frame(
    simulation_day = 8,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24920,
    n_latent = 43,
    n_subclinical = 8,
    n_clinical = 13,
    n_recovered = 5,
    n_dead = 11,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_8_infected_net_pen_info <- he_simulate_day(
    day_7_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 8,
    test_species_info
  )
  expect_equal(
    day_8_infected_net_pen_info,
    expected_day_8_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_8_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(10, 9, 2, 7, 4, 6, 5, 0),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(5, 3, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(2, 0, 2, 3, 0, 3, 0, 3),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_8_disease_stage_duration_matrices
  )

  # Day 9 ---
  # Check infected net pen info
  expected_day_9_infected_net_pen_info <- data.frame(
    simulation_day = 9,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24899,
    n_latent = 54,
    n_subclinical = 7,
    n_clinical = 17,
    n_recovered = 10,
    n_dead = 13,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_9_infected_net_pen_info <- he_simulate_day(
    day_8_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 9,
    test_species_info
  )
  expect_equal(
    day_9_infected_net_pen_info,
    expected_day_9_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_9_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(9, 2, 7, 4, 9, 12, 6, 5),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(4, 1, 2, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(1, 2, 3, 1, 6, 1, 3, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_9_disease_stage_duration_matrices
  )

  # Day 10 ---
  # Check infected net pen info
  expected_day_10_infected_net_pen_info <- data.frame(
    simulation_day = 10,
    net_pen_id = 1,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = 24877,
    n_latent = 67,
    n_subclinical = 6,
    n_clinical = 22,
    n_recovered = 14,
    n_dead = 14,
    n_total = 25000,
    infection_origin = "index",
    day_infected = 0
  )
  day_10_infected_net_pen_info <- he_simulate_day(
    day_9_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 10,
    test_species_info
  )
  expect_equal(
    day_10_infected_net_pen_info,
    expected_day_10_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_10_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(2, 7, 4, 9, 14, 10, 13, 8),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(2, 4, 0, 0, 0, 0, 0, 0),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(2, 4, 2, 7, 1, 4, 2, 0),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_10_disease_stage_duration_matrices
  )
})

test_that("day is simulated correctly for a single index net pen with aggressive between net pen infection spread", {
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

  test_n_index_infected_min <- 20000
  test_n_index_infected_mode <- 20000
  test_n_index_infected_max <- 20000
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

  # Simulate 5 days of infection dynamics ---
  # Day 1 ---
  # Check infected net pen info
  expected_day_1_infected_net_pen_info <- data.frame(
    simulation_day = 1,
    net_pen_id = c(1, 10, 11, 14, 19, 20),
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = c(2300, rep(24900, 5)),
    n_latent = c(2700, rep(100, 5)),
    n_subclinical = c(6010, rep(0, 5)),
    n_clinical = c(10569, rep(0, 5)),
    n_recovered = c(1990, rep(0, 5)),
    n_dead = c(1431, rep(0, 5)),
    n_total = 25000,
    infection_origin = c("index", rep("between-net pen", 5)),
    day_infected = c(0, rep(1, 5))
  )
  day_1_infected_net_pen_info <- he_simulate_day(
    test_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 1,
    test_species_info
  )
  expect_equal(
    day_1_infected_net_pen_info,
    expected_day_1_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_1_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(
          c(0, 0, 2, 28, 261, 650, 966, 793),
          c(0, 0, 0, 5, 22, 30, 43, 0),
          c(0, 0, 1, 11, 28, 32, 28, 0),
          c(0, 0, 1, 6, 23, 32, 38, 0),
          c(0, 0, 1, 12, 26, 35, 26, 0),
          c(0, 0, 2, 13, 28, 28, 29, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(
          c(4022, 1988, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(
          c(1481, 1500, 1524, 1539, 1493, 1485, 1547, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_1_disease_stage_duration_matrices
  )

  # Day 2 ---
  # Check infected net pen info
  expected_day_2_infected_net_pen_info <- data.frame(
    simulation_day = 2,
    net_pen_id = c(1, 10, 11, 14, 19, 20, 16),
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = c(1059, rep(24900, 6)),
    n_latent = c(3941, rep(100, 6)),
    n_subclinical = c(1988, rep(0, 6)),
    n_clinical = c(9088, rep(0, 6)),
    n_recovered = c(6012, rep(0, 6)),
    n_dead = c(2912, rep(0, 6)),
    n_total = 25000,
    infection_origin = c("index", rep("between-net pen", 6)),
    day_infected = c(0, rep(1, 5), 2)
  )
  day_2_infected_net_pen_info <- he_simulate_day(
    day_1_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 2,
    test_species_info
  )
  expect_equal(
    day_2_infected_net_pen_info,
    expected_day_2_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_2_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(
          c(0, 2, 28, 273, 769, 1260, 1234, 375),
          c(0, 0, 5, 22, 30, 43, 0, 0),
          c(0, 1, 11, 28, 32, 28, 0, 0),
          c(0, 1, 6, 23, 32, 38, 0, 0),
          c(0, 1, 12, 26, 35, 26, 0, 0),
          c(0, 2, 13, 28, 28, 29, 0, 0),
          c(0, 0, 1, 10, 22, 36, 31, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(
          c(1988, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(
          c(1500, 1524, 1539, 1493, 1485, 1547, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_2_disease_stage_duration_matrices
  )

  # Day 3 ---
  # Check infected net pen info
  expected_day_3_infected_net_pen_info <- data.frame(
    simulation_day = 3,
    net_pen_id = c(1, 10, 11, 14, 19, 20, 16, 4),
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = c(550, rep(24900, 7)),
    n_latent = c(4450, rep(100, 7)),
    n_subclinical = c(0, rep(0, 7)),
    n_clinical = c(7588, rep(0, 7)),
    n_recovered = c(8000, rep(0, 7)),
    n_dead = c(4412, rep(0, 7)),
    n_total = 25000,
    infection_origin = c("index", rep("between-net pen", 7)),
    day_infected = c(0, rep(1, 5), 2, 3)
  )
  day_3_infected_net_pen_info <- he_simulate_day(
    day_2_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 3,
    test_species_info
  )
  expect_equal(
    day_3_infected_net_pen_info,
    expected_day_3_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_3_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(
          c(2, 28, 273, 775, 1316, 1353, 532, 171),
          c(0, 5, 22, 30, 43, 0, 0, 0),
          c(1, 11, 28, 32, 28, 0, 0, 0),
          c(1, 6, 23, 32, 38, 0, 0, 0),
          c(1, 12, 26, 35, 26, 0, 0, 0),
          c(2, 13, 28, 28, 29, 0, 0, 0),
          c(0, 1, 10, 22, 36, 31, 0, 0),
          c(0, 0, 0, 10, 25, 41, 24, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(
          c(1524, 1539, 1493, 1485, 1547, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_3_disease_stage_duration_matrices
  )

  # Day 4 ---
  # Check infected net pen info
  expected_day_4_infected_net_pen_info <- data.frame(
    simulation_day = 4,
    net_pen_id = c(1, 10, 11, 14, 19, 20, 16, 4, 13),
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = c(307, rep(24900, 8)),
    n_latent = c(4691, 100, 99, 99, 99, 98, 100, 100, 100),
    n_subclinical = c(0, rep(0, 8)),
    n_clinical = c(6066, 0, 1, 1, 1, 2, 0, 0, 0),
    n_recovered = c(8000, rep(0, 8)),
    n_dead = c(5936, rep(0, 8)),
    n_total = 25000,
    infection_origin = c("index", rep("between-net pen", 8)),
    day_infected = c(0, rep(1, 5), 2, 3, 4)
  )
  day_4_infected_net_pen_info <- he_simulate_day(
    day_3_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 4,
    test_species_info
  )
  expect_equal(
    day_4_infected_net_pen_info,
    expected_day_4_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_4_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(
          c(28, 273, 775, 1318, 1374, 597, 263, 63),
          c(5, 22, 30, 43, 0, 0, 0, 0),
          c(11, 28, 32, 28, 0, 0, 0, 0),
          c(6, 23, 32, 38, 0, 0, 0, 0),
          c(12, 26, 35, 26, 0, 0, 0, 0),
          c(13, 28, 28, 29, 0, 0, 0, 0),
          c(1, 10, 22, 36, 31, 0, 0, 0),
          c(0, 0, 10, 25, 41, 24, 0, 0),
          c(0, 0, 3, 14, 17, 43, 23, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(
          c(1539, 1493, 1485, 1548, 0, 0, 1, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 1, 0, 0, 0, 0),
          c(1, 0, 0, 0, 0, 0, 0, 0),
          c(0, 1, 0, 0, 0, 0, 0, 0),
          c(1, 0, 1, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_4_disease_stage_duration_matrices
  )

  # Day 5 ---
  # Check infected net pen info
  expected_day_5_infected_net_pen_info <- data.frame(
    simulation_day = 5,
    net_pen_id = c(1, 10, 11, 14, 19, 20, 16, 4, 13),
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 1,
    n_susceptible = c(
      182,
      24900,
      24898,
      24899,
      24898,
      24899,
      24900,
      24900,
      24900
    ),
    n_latent = c(4788, 95, 90, 94, 89, 86, 99, 100, 100),
    n_subclinical = c(11, 2, 4, 2, 4, 5, 0, 0, 0),
    n_clinical = c(4544, 3, 8, 4, 9, 9, 1, 0, 0),
    n_recovered = c(8000, rep(0, 8)),
    n_dead = c(7475, 0, 0, 1, 0, 1, 0, 0, 0),
    n_total = 25000,
    infection_origin = c("index", rep("between-net pen", 8)),
    day_infected = c(0, rep(1, 5), 2, 3, 4)
  )
  day_5_infected_net_pen_info <- he_simulate_day(
    day_4_infected_net_pen_info,
    test_simulation_env,
    simulation_day = 5,
    test_species_info
  )
  expect_equal(
    day_5_infected_net_pen_info,
    expected_day_5_infected_net_pen_info
  )
  # Check disease stage duration matrices
  expected_day_5_disease_stage_duration_matrices <-
    list(
      latent_duration = matrix(
        c(
          c(273, 775, 1318, 1378, 605, 297, 103, 39),
          c(22, 30, 43, 0, 0, 0, 0, 0),
          c(28, 32, 28, 0, 0, 0, 1, 1),
          c(23, 32, 38, 0, 0, 0, 1, 0),
          c(26, 35, 26, 0, 0, 0, 1, 1),
          c(28, 28, 29, 1, 0, 0, 0, 0),
          c(10, 22, 36, 31, 0, 0, 0, 0),
          c(0, 10, 25, 41, 24, 0, 0, 0),
          c(0, 3, 14, 17, 43, 23, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      subclinical_duration = matrix(
        c(
          c(4, 5, 2, 0, 0, 0, 0, 0),
          c(1, 1, 0, 0, 0, 0, 0, 0),
          c(2, 2, 0, 0, 0, 0, 0, 0),
          c(1, 1, 0, 0, 0, 0, 0, 0),
          c(1, 2, 1, 0, 0, 0, 0, 0),
          c(2, 3, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      ),
      clinical_duration = matrix(
        c(
          c(1496, 1486, 1551, 3, 1, 3, 3, 1),
          c(0, 0, 0, 0, 1, 0, 0, 2),
          c(1, 0, 1, 2, 0, 1, 3, 0),
          c(0, 1, 0, 0, 0, 1, 0, 2),
          c(3, 2, 0, 2, 0, 1, 1, 0),
          c(0, 2, 2, 0, 1, 3, 1, 0),
          c(0, 0, 0, 0, 1, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0),
          c(0, 0, 0, 0, 0, 0, 0, 0)
        ),
        ncol = 8,
        byrow = TRUE
      )
    )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices,
    expected_day_5_disease_stage_duration_matrices
  )
})
