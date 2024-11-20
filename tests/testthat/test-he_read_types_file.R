test_that("types file reads in with correct values", {
  filepath <- system.file("testdata", package = "hydroepixr")

  dat <- he_read_types_file(paste0(filepath, "/types_file_bay_x.csv"))

  expect_no_error(dat)
  expected_dat <- data.frame(farm_type_id = 1,
                             farm_type_name = "Fish1",
                             lat_dur_freq = "c(0.000001,0.000001,0.0001,0.01,0.1,0.25,0.339898,0.3)",
                             sub_dur_freq = "c(0.249995,0.50,0.25,0.000001,0.000001,0.000001,0.000001,0.000001)",
                             cli_dur_freq = "c(0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125)",
                             within_pen_transmission = "rpert(n,0.14,0.4,0.8)",
                             rel_susceptibility = 1)
  mismatched_values <- which(dat != expected_dat)
  expect_length(mismatched_values, 0)
})

test_that("types file without correct headers throws error", {
  filepath <- system.file("testdata", package = "hydroepixr")
  expect_error(he_read_types_file(paste0(
    filepath,
    "/types_file_bay_x_wrong_headers.csv"
  )),
  regexp = "Unexpected column headers. Expected headers are: ")
})
