test_that("default output parameters are stored in the environment", {
  test_environment <- rlang::new_environment()

  he_define_output_params(test_environment)

  expect_equal(test_environment$output_dir, "output")
  expect_equal(test_environment$model_run_id, NULL)
  expect_equal(test_environment$inf_netpen_output_file_name,
               "infected_netpens.csv")
})

test_that("user-defined output parameters are stored in the environment", {
  test_output_dir <- "simulation_output_data"
  test_model_run_id <- "2025-01-10"
  test_inf_netpen_output_file_name <- "infected_farm_output_file.csv"

  test_environment <- rlang::new_environment()
  he_define_output_params(
    test_environment,
    test_output_dir,
    test_model_run_id,
    test_inf_netpen_output_file_name
  )

  expect_equal(test_environment$output_dir, test_output_dir)
  expect_equal(test_environment$model_run_id, test_model_run_id)
  expect_equal(test_environment$inf_netpen_output_file_name,
               test_inf_netpen_output_file_name)
})
