test_that("netpen info file reads in with correct values", {
  filepath <- system.file("testdata", package = "hydroepixr")

  dat <- he_read_netpen_info_file(paste0(filepath, "/netpen_file_bay_x.csv"))

  expect_no_error(dat)
  expected_dat <- readRDS(paste0(filepath, "/raw_netpen_info_bay_x.rds"))
  mismatched_values <- which(dat != expected_dat)
  expect_length(mismatched_values, 0)
})

test_that("netpen info file without correct headers throws error", {
  filepath <- system.file("testdata", package = "hydroepixr")
  expect_error(he_read_netpen_info_file(paste0(
    filepath,
    "/netpen_file_bay_x_wrong_headers.csv"
  )),
  regexp = "Unexpected column headers. Expected headers are: ")
})

test_that("netpen info file with duplicate netpen identifiers throws error", {
  filepath <- system.file("testdata", package = "hydroepixr")
  expect_error(he_read_netpen_info_file(paste0(
    filepath,
    "/netpen_file_bay_x_duplicate_netpen_ids.csv"
  )),
  regexp = "Netpen ID numbers are not unique. Simulations Fails. Duplicate Value: ")
})
