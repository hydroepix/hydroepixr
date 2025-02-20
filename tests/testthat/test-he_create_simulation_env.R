test_that("environment is generated", {
  test_parent_env <- rlang::new_environment()
  test_env <- he_create_simulation_env(test_parent_env)
  expect_true(is.environment(test_env))
})

test_that("generated environment has specified environment as parent", {
  test_parent_env <- rlang::new_environment()
  test_env <- he_create_simulation_env(test_parent_env)
  expect_equal(rlang::env_parent(test_env), test_parent_env)
})
