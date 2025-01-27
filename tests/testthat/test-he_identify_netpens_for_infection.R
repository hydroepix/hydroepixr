test_that("potentially infected netpens are correctly identified", {
  # Simplified farm info with only the columns relevant for this function
  test_farm_info <- as.list(data.frame(netpen_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               farm_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                               netpen_size = rep(25000, 9),
                               status = c(1, 1, 0, 1, 1, 1, 1, 0, 1),
                               farm_active = c(T, T, F, T, F, T, T, T, T)))
  test_inf_farm_id <- 1
  actual <- he_identify_netpens_for_infection(test_farm_info, test_inf_farm_id)
  expected <-  c(1, 2)
  expect_equal(actual, expected)

  test_inf_farm_id <- 2
  actual <- he_identify_netpens_for_infection(test_farm_info, test_inf_farm_id)
  expected <- c(4, 6)
  expect_equal(actual, expected)
})
