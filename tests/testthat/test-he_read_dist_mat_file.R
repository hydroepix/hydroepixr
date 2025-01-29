test_that("distance matrix file reads in with correct values", {
  filepath <- system.file("testdata", package = "hydroepixr")

  dat <- he_read_dist_mat_file(paste0(filepath, "/dist_mat_bay_x.csv"))

  expect_no_error(dat)
  expected_dat <- data.frame(c(0, 2000, 7000),
                             c(2000, 0, 5000),
                             c(7000, 5000, 0))
  mismatched_values <- which(dat != expected_dat)
  expect_length(mismatched_values, 0)
})

test_that("distance matrix file provides symmetrical matrix", {
  # TODO
})

# TODO: Add checks for invalid values e.g. negative numbers, non-numerics
