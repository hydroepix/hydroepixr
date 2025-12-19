test_that("infected net pen info is initialized correctly", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  test_output_file_name <- "infected_net_pens.csv"

  test_infected_net_pen_info <- he_initialize_infected_net_pen_info(
    temp_test_dir,
    test_output_file_name
  )
  expect_no_error(test_infected_net_pen_info)
  # Check column names and types
  infected_net_pen_info_columns <- data.frame(
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
    is_vaccinated = double()
  )
  # Check that column names and types are correct
  expect_equal(
    names(test_infected_net_pen_info),
    names(infected_net_pen_info_columns)
  )
  expect_equal(
    sapply(test_infected_net_pen_info, class),
    sapply(infected_net_pen_info_columns, class)
  )
  # Check that column names have been written to output file
  test_output_file_data <- read.csv(file.path(
    temp_test_dir,
    test_output_file_name
  ))
  expect_equal(
    names(test_output_file_data),
    names(infected_net_pen_info_columns)
  )
})
