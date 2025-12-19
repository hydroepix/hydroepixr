test_that("net pen info file reads in with correct values", {
  filepath <- system.file("testdata", package = "hydroepixr")

  dat <- he_read_net_pen_info_file(paste0(filepath, "/net_pen_file_bay_x.csv"))

  expect_no_error(dat)
  expected_dat <- readRDS(paste0(filepath, "/raw_net_pen_info_bay_x.rds"))
  mismatched_values <- which(dat != expected_dat)
  expect_length(mismatched_values, 0)
})

test_that("net pen info file without correct headers throws error", {
  filepath <- system.file("testdata", package = "hydroepixr")
  expect_error(
    he_read_net_pen_info_file(paste0(
      filepath,
      "/net_pen_file_bay_x_wrong_headers.csv"
    )),
    regexp = "Unexpected column headers. Expected headers are: "
  )
})

test_that("net pen info file with duplicate net pen identifiers throws error", {
  filepath <- system.file("testdata", package = "hydroepixr")
  expect_error(
    he_read_net_pen_info_file(paste0(
      filepath,
      "/net_pen_file_bay_x_duplicate_net_pen_ids.csv"
    )),
    regexp = "Net pen ID numbers are not unique. Simulations Fails. Duplicate Value: "
  )
})
