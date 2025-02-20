test_that("species file reads in with correct values", {
  filepath <- system.file("testdata", package = "hydroepixr")

  dat <- he_read_species_info_file(paste0(filepath, "/species_info_file_bay_x.csv"))

  expect_no_error(dat)
  expected_dat <- readRDS(paste0(filepath, "/parsed_species_info_bay_x.rds"))

  expect_equal(dat, expected_dat)
})

test_that("species info file without correct headers throws error", {
  filepath <- system.file("testdata", package = "hydroepixr")
  expect_error(he_read_species_info_file(paste0(
    filepath,
    "/species_info_file_bay_x_wrong_headers.csv"
  )),
  regexp = "Unexpected column headers. Expected headers are: ")
})


test_that("species ids are unique", {
  # TODO
  # filepath <- system.file("testdata", package = "hydroepixr")
  # expect_error(he_read_species_info_file(paste0(
  #   filepath,
  #   "/species_info_file_bay_x_duplicate_species_ids.csv"
  # )),
  # regexp = "Species ID numbers are not unique. Simulations Fails. Duplicate Value: ")
})
