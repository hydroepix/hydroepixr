test_that("net pen info file reads in with correct values", {
  dat <- he_read_net_pen_info_file(test_net_pen_info_filepath)
  expect_no_error(dat)
  expected_dat <- readRDS(paste0(filepath, "/raw_net_pen_info_bay_x.rds"))
  mismatched_values <- which(dat != expected_dat)
  expect_length(mismatched_values, 0)
})

test_that("net pen info file without correct headers throws error", {
  expect_error(
    he_read_net_pen_info_file(paste0(
      test_data_filepath,
      "/net_pen_file_bay_x_wrong_headers.csv"
    )),
    regexp = "Error: Unexpected column headers. Expected headers are: "
  )
})

test_that("net pen info file with duplicate net pen identifiers throws error", {
  expect_error(
    he_read_net_pen_info_file(paste0(
      test_data_filepath,
      "/net_pen_file_bay_x_duplicate_net_pen_ids.csv"
    )),
    regexp = "Error: simulation failed. Net pen ID must be unique, even across farms. Duplicate Value: "
  )
})
