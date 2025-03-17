test_that("simulation environment variables are correctly initialized", {
  test_simulation_env <- rlang::new_environment()
  test_species_info <- readRDS(paste0(test_data_filepath,
                                      "/parsed_species_info_bay_x.rds"))
  he_initialize_simulation_env(test_simulation_env, test_species_info)

  expect_true(exists("disease_stage_duration_matrices",
                     test_simulation_env))

  expect_equal(test_simulation_env$sim_day, 0)

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
