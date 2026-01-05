test_that("simulation environment variables are correctly initialized", {
  test_simulation_env <- rlang::new_environment()
  # Create test directory
  temp_test_dir <- output_test_setup()
  test_infected_net_pen_output_file_name <- "infected_net_pens.csv"
  test_species_info <- readRDS(paste0(
    test_data_filepath,
    "/parsed_species_info_bay_x.rds"
  ))
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_model_run_id <- "testmodel"
  test_net_pen_to_net_pen <- 0.5
  test_clinically_infected_prop <- 0.6
  test_simulation_n <- 10
  he_initialize_simulation_env(
    test_simulation_env,
    test_species_info,
    test_net_pen_info,
    test_net_pen_to_net_pen,
    output_dir = temp_test_dir,
    test_model_run_id,
    test_infected_net_pen_output_file_name,
    test_clinically_infected_prop,
    test_simulation_n
  )

  expected_filepath <-
    file.path(
      temp_test_dir,
      paste(
        test_model_run_id,
        test_simulation_n,
        test_infected_net_pen_output_file_name,
        sep = "_"
      )
    )

  expect_true(file.exists(expected_filepath))

  # Check that column names have been written to output file
  infected_net_pen_info_cols <- data.frame(
    simulation_day = integer(),
    net_pen_id = integer(),
    farm_id = integer(),
    species_id = integer(),
    within_net_pen_transmission = double(),
    n_susceptible = integer(),
    n_latent = integer(),
    n_subclinical = integer(),
    n_clinical = integer(),
    n_recovered = integer(),
    n_dead = integer(),
    n_total = integer(),
    infection_origin = character(),
    day_infected = double(),
    is_vaccinated = numeric()
  )
  test_output_file_data <- read.csv(expected_filepath)
  expect_equal(names(test_output_file_data), names(infected_net_pen_info_cols))

  expect_true(exists("disease_stage_duration_matrices", test_simulation_env))

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
