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
  # TODO: Set a random seed to ensure expected values for new infections,
  # newly infected animal disease stage durations
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

  test_index_netpens <- 1
  test_infected_netpen_info <-
    he_initialize_infection(
      test_infected_netpen_info,
      test_simulation_env,
      test_n_index_infected_min,
      test_n_index_infected_mode,
      test_n_index_infected_max,
      test_species_info,
      test_netpen_info,
      test_index_netpens
    )
  # Simulate 10 days of infection dynamics
  result_infected_netpen_info <- he_simulate_day(test_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 1,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 2,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 3,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 4,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 5,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 6,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 7,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 8,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 9,
                                          test_species_info)
  result_infected_netpen_info <- he_simulate_day(result_infected_netpen_info,
                                          test_simulation_env,
                                          simulation_day = 10,
                                          test_species_info)
  # TODO:
  # Check correct simulation day is recorded
  # Check disease stage matrices (in simulation env)
  # Check updated disease stage counts
})
