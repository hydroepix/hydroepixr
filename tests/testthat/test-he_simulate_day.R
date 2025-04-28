test_that("simulating a day with no remaining infections generates error", {
  test_inf_farm_info <-
    readRDS(paste0(test_data_filepath, "/inf_farm_info_initialized.rds"))
  test_species_info <- test_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_simulation_env <- rlang::new_environment()
  # Set up test output directory and file, initialize test environment
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")

  expect_error(he_simulate_day(test_inf_farm_info, test_simulation_env),
               regexp = "No remaining infected netpens. Simulation should have terminated.")
})

test_that("day is simulated correctly for a single infected farm", {
  # TODO: Set a random seed to ensure expected values for new infections,
  # newly infected animal disease stage durations
  withr::local_seed(100)
  test_inf_farm_info <-
    readRDS(paste0(test_data_filepath, "/inf_farm_info_initialized.rds"))
  test_species_info <-
    readRDS(paste0(test_data_filepath, "/species_info_guaranteed_infection.rds"))
  test_farm_info <-
    readRDS(paste0(test_data_filepath, "/initialized_farm_info_guaranteed_infection.rds"))
  test_simulation_env <- rlang::new_environment()
  # Set up test output directory and file, initialize test environment
  temp_test_dir <-
    output_and_simulation_env_test_setup(test_simulation_env,
                                         test_species_info,
                                         test_output_file_name = "infected_netpens.csv")


  test_index_netpens <- 1
  test_inf_farm_info <-
    he_initialize_infection(
      test_inf_farm_info,
      test_simulation_env,
      test_species_info,
      test_farm_info,
      test_index_netpens
    )
  print(test_inf_farm_info)
  print("\n")

  result_inf_farm_info <- he_simulate_day(test_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  result_inf_farm_info <- he_simulate_day(result_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  result_inf_farm_info <- he_simulate_day(result_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  result_inf_farm_info <- he_simulate_day(result_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  result_inf_farm_info <- he_simulate_day(result_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  result_inf_farm_info <- he_simulate_day(result_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  result_inf_farm_info <- he_simulate_day(result_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  result_inf_farm_info <- he_simulate_day(result_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  result_inf_farm_info <- he_simulate_day(result_inf_farm_info,
                                          test_simulation_env,
                                          test_species_info)
  print(result_inf_farm_info)
  print("\n")
  # Check disease stage matrices (in simulation env)
  # Check updated disease stage counts
})
