test_that("infected netpen info is initialized correctly", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  test_output_file_name <- "infected_netpens.csv"

  test_infected_netpen_info <- he_initialize_infected_netpen_info(temp_test_dir,
                                                    test_output_file_name)
  expect_no_error(test_infected_netpen_info)
  # Check column names and types
  infected_netpen_info_columns <- data.frame(
    simulation_day = integer(),
    netpen_id = integer(),
    farm_id = integer(),
    species_id = integer(),
    within_netpen_transmission = double(),
    n_susceptible = integer(),
    n_latent = integer(),
    n_subclinical = integer(),
    n_clinical = integer(),
    n_recovered = integer(),
    n_dead = integer(),
    n_total = integer(),
    infection_origin = character(),
    day_infected = double(),
    is_vaccinated = double()
  )
  # Check that column names and types are correct
  expect_equal(names(test_infected_netpen_info),
               names(infected_netpen_info_columns))
  expect_equal(sapply(test_infected_netpen_info, class),
               sapply(infected_netpen_info_columns, class))
  # Check that column names have been written to output file
  test_output_file_data <- read.csv(file.path(temp_test_dir,
                                              test_output_file_name))
  expect_equal(names(test_output_file_data), names(infected_netpen_info_columns))
})
