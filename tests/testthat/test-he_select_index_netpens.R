test_that("invalid netpen IDs produces error", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  expect_error(he_select_index_netpens(farm_info,
                                    netpen_ids = c(56, 57, 58, 59, 60, 61)),
               regex = "Invalid netpen ID identified. Please confirm all provided netpen
             IDs exist in the farm info file.")
})

test_that("lack of netpen or farm ID produces error", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  expect_error(he_select_index_netpens(farm_info))
})

test_that("invalid darm ID produces error", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  expect_error(he_select_index)
})

test_that("randomized index netpen selection from farm ID works", {

})

test_that("index netpen selection from netpen ID works", {
})
