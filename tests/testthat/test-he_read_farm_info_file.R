test_that("farm info file reads in with correct values", {
  filepath <- system.file("testdata", package = "hydroepixr")

  dat <- he_read_farm_info_file(paste0(filepath, "/farm_file_bay_x.csv"))

  expect_no_error(dat)
  expected_dat <- data.frame(netpen_id = c(1:60),
                             farm_id = c(rep(1, 20), rep(2, 20), rep(3, 20)),
                             netpen_size = rep(25000, 60),
                             baseline_mort = c(
                               0.000107248,
                               0.000121311,
                               0.000175868,
                               0.0002725,
                               0.000179603,
                               0.000293589,
                               0.000156196,
                               0.000134537,
                               0.000173224,
                               0.000172327,
                               0.000186443,
                               0.000164296,
                               0.000217816,
                               0.000134207,
                               0.000158678,
                               0.000233801,
                               0.000162511,
                               0.000159212,
                               0.000184866,
                               0.00013313,
                               0.000186295,
                               0.0001802,
                               0.000155842,
                               0.000165952,
                               0.000291346,
                               0.0001846,
                               0.000215966,
                               0.000162263,
                               0.000118637,
                               0.000231923,
                               0.000229088,
                               0.000165418,
                               0.000251452,
                               0.000167724,
                               0.000154064,
                               0.000232146,
                               0.00013427,
                               0.000178447,
                               0.00018037,
                               0.000228731,
                               0.000151376,
                               0.00028235,
                               0.000174696,
                               0.000159531,
                               0.000218316,
                               0.000153157,
                               0.000130646,
                               0.000137381,
                               0.000140383,
                               0.000158229,
                               0.000179011,
                               0.000181484,
                               0.000113841,
                               0.000151699,
                               0.000237151,
                               0.000161233,
                               0.000162239,
                               0.00022594,
                               0.000162843,
                               0.000137203
                             ),
                             species = rep(1, 60),
                             bmaid = rep(1, 60))
  mismatched_values <- which(dat != expected_dat)
  expect_length(mismatched_values, 0)
})

test_that("farm info file without correct headers throws error", {
  filepath <- system.file("testdata", package = "hydroepixr")
  expect_error(he_read_farm_info_file(paste0(
    filepath,
    "/farm_file_bay_x_wrong_headers.csv"
  )),
  regexp = "Unexpected column headers. Expected headers are: ")
})

test_that("farm info file with duplicate netpen identifiers throws error", {
  filepath <- system.file("testdata", package = "hydroepixr")
  expect_error(he_read_farm_info_file(paste0(
    filepath,
    "/farm_file_bay_x_duplicate_netpen_ids.csv"
  )),
  regexp = "Netpen ID numbers are not unique. Simulations Fails. Duplicate Value: ")
})
