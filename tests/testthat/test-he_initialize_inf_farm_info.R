test_that("infected farm info is initialized correctly", {
  # Create test directory
  temp_test_dir <- output_test_setup()
  test_output_file_name <- "infected_netpens.csv"

  test_inf_farm_info <- he_initialize_inf_farm_info(temp_test_dir,
                                                    test_output_file_name)
  expect_no_error(test_inf_farm_info)
  # Check column names and types
  inf_farm_info_columns <- data.frame(
    simulation_day = integer(),
    netpen_id = integer(),
    farm_id = integer(),
    species_id = integer(),
    within_netpen_transmission = double(),
    susceptible = integer(),
    latent = integer(),
    subclinical = integer(),
    clinical = integer(),
    immune = integer(),
    total = integer(),
    infection_status = integer(),
    latent_duration = double(),
    subclinical_duration = double(),
    clinical_time = double(),
    time_of_diagnosis = double(),
    diagnosed = logical(),
    infected_by_direct_contact = character(),
    time_infected = double(),
    vaccinated = numeric()
  )
  # Check that column names and types are correct
  expect_equal(names(test_inf_farm_info),
               names(inf_farm_info_columns))
  expect_equal(sapply(test_inf_farm_info, class),
               sapply(inf_farm_info_columns, class))
  # Check that column names have been written to output file
  test_output_file_data <- read.csv(file.path(temp_test_dir,
                                              test_output_file_name))
  expect_equal(names(test_output_file_data), names(inf_farm_info_columns))
})
