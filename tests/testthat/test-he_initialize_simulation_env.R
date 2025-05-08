test_that("simulation environment variables are correctly initialized", {
  test_simulation_env <- rlang::new_environment()
  # Create test directory
  temp_test_dir <- output_test_setup()
  test_inf_netpen_output_file_name <- "infected_netpens.csv"
  test_species_info <- readRDS(paste0(test_data_filepath,
                                      "/parsed_species_info_bay_x.rds"))
  test_model_run_id <- "testmodel"
  test_simulation_num <- 10
  he_initialize_simulation_env(
    test_simulation_env,
    test_species_info,
    output_dir = temp_test_dir,
    test_model_run_id,
    test_inf_netpen_output_file_name,
    test_simulation_num
  )

  expected_filepath <-
    file.path(
      temp_test_dir,
      paste(
        test_model_run_id,
        test_simulation_num,
        test_inf_netpen_output_file_name,
        sep = "_"
      )
    )

  expect_true(file.exists(expected_filepath))

  # Check that column names have been written to output file
  inf_farm_info_columns <- data.frame(
    simulation_day = integer(),
    netpen_id = integer(),
    farm_id = integer(),
    species_id = integer(),
    within_netpen_transmission = double(),
    n_susceptible = integer(),
    n_latent = integer(),
    n_subclinical = integer(),
    n_clinical = integer(),
    n_immune = integer(),
    n_total = integer(),
    infection_status = integer(),
    infection_origin = character(),
    day_infected = double(),
    is_vaccinated = numeric()
  )
  test_output_file_data <- read.csv(expected_filepath)
  expect_equal(names(test_output_file_data), names(inf_farm_info_columns))

  expect_true(exists("disease_stage_duration_matrices",
                     test_simulation_env))

  expect_equal(
    test_simulation_env$disease_stage_duration_matrices$latent_duration,
    matrix(numeric(0), ncol = 8)
  )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices$subclinical_duration,
    matrix(numeric(0), ncol = 8)
  )
  expect_equal(
    test_simulation_env$disease_stage_duration_matrices$clinical_duration,
    matrix(numeric(0), ncol = 8)
  )
})
