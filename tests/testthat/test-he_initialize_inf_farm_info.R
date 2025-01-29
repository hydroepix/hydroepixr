test_that("infected farm info is initialized correctly", {
  # Create test simulation environment
  test_environment <- rlang::new_environment()
  # Set other environment variables on which infected farm info is dependent
  test_environment$days_dead_infectious <- 2
  test_environment$past_days_for_dead_animal_surveillance <- 7
  expect_no_error(he_initialize_inf_farm_info(test_environment))
  # Set complex expected values
  expected_inf_farm_cols <- c(
    "farm_id",
    "species_id",
    "susceptible",
    "latent",
    "subclinical",
    "clinical",
    "immune",
    "total", #???
    "infection_status", #???
    #"p", # Reed-Frost probability?
    "latent_duration", # Length of time in latent stage?
    "subclinical_duration", # Length of time in subclinical stage?
    "clinical_time", # time the herd showed clinical signs of infection
    "time_of_diagnosis",
    "diagnosed",
    "infected_by_direct_contact",
    "time_infected", # appears to refer to the timestep of infection
    "vaccinated"#,
    #"TLastAnCli19" #???
  )
  expected_mort_matrix_cols <-
    c("iteration", "inf_farm_id", "inf_netpen_id", "mort", "day")
  # Check environment variables are set to correct values
  expect_equal(test_environment$inf_farm_cols, expected_inf_farm_cols)
  expect_equal(test_environment$inf_farms,
               matrix(numeric(0), ncol = length(expected_inf_farm_cols)))
  expect_equal(test_environment$dead_animal_matrix,
               matrix(numeric(0), ncol = test_environment$days_dead_infectious))
  expect_equal(
    test_environment$dead_animal_matrix_sur,
    matrix(numeric(0), ncol = test_environment$past_days_for_dead_animal_surveillance)
  )
  expect_equal(test_environment$mort_matrix_cols, expected_mort_matrix_cols)
  expect_equal(test_environment$mort_matrix, matrix(numeric(0), ncol = 5))
  expect_equal(test_environment$deleted_farm_matrix,
               matrix(numeric(0), length(expected_inf_farm_cols)))
})
