test_that("environment is generated", {
  test_env <- he_initialize_simulation_env()
  expect_true(is.environment(test_env))
})

test_that("generated environment has empty environment as parent", {
  test_env <- he_initialize_simulation_env()
  expect_equal(rlang::env_parent(test_env), rlang::empty_env())
})
