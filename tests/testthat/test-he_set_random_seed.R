test_that("check positive seed input is calculated correctly", {
  test_seed <- he_set_random_seed(1, 10)
  expect_equal(test_seed, 1)
})

test_that("check zero seed input is calculated correctly", {
  test_seed <- he_set_random_seed(0, 10)
  expect_equal(test_seed, 0)
})

test_that("check negative ssed input is calculated correctly", {
  test_seed <- he_set_random_seed(-1, 10)
  expect_equal(test_seed, 11)
})
