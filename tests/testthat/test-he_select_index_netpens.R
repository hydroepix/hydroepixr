test_that("invalid netpen IDs produces error", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  expect_error(he_select_index_netpens(farm_info,
                                    netpen_ids = c(56, 57, 58, 59, 60, 61)),
               regex = "Invalid netpen ID identified. Please confirm all provided netpen
             IDs exist in the farm info file.")
})

test_that("lack of netpen or farm ID produces error", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  expect_error(he_select_index_netpens(farm_info),
               regex = "Either a specific netpen ID or a farm ID must be provided in order
             to select an index netpen.")
})

test_that("invalid farm ID produces error", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  expect_error(he_select_index_netpens(farm_info, farm_id = 4),
               regex = "Farm ID provided not found in farm info. Please confirm the
             provided farm ID exists in the farm info file.")
})

test_that("providing both netpen ID and farm ID produces error", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  expect_error(he_select_index_netpens(farm_info,
                                       netpen_ids = c(1, 2, 3, 4, 5),
                                       farm_id = 3),
               regex = "Either netpen ID or farm ID should be provided, not both.")
})

test_that("randomized index netpen selection from farm ID works with default
          of 1 netpen", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  test_index_netpens <- he_select_index_netpens(farm_info, farm_id = 3)
  # Check the appropriate number of netpens have been selected
  expect_equal(length(test_index_netpens), 1)
  # Check that the selected netpen exists in the data frame
  test_index_netpen_row <- subset(farm_info,
                                  farm_info$netpen_id %in% test_index_netpens)
  expect_equal(nrow(test_index_netpen_row), 1)
})

test_that("randomixed index netpen selection from farm ID works with specified
          number of netpens", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  test_index_netpens <- he_select_index_netpens(farm_info,
                                                farm_id = 2,
                                                n_inf_netpens = 3)
  # Check the appropriate number of netpens have been selected
  expect_equal(length(test_index_netpens), 3)
  # Check that the selected netpens exist in the data frame
  test_index_netpen_rows <- subset(farm_info,
                                   farm_info$netpen_id %in% test_index_netpens)
  expect_equal(nrow(test_index_netpen_rows), 3)
  # Check that the selected netpens exist as part of the specified farm
  expect_true(all(test_index_netpen_rows$farm_id == 2))
})

test_that("index netpen selection from netpen ID works", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  test_index_netpens <- he_select_index_netpens(farm_info,
                                                netpen_ids = c(45, 46, 47, 48, 49))
  # Check the appropriate number of netpens have been selected
  expect_equal(length(test_index_netpens), 5)
  # Check that the selected netpens exist in the data frame
  test_index_netpen_rows <- subset(farm_info,
                                   farm_info$netpen_id %in% test_index_netpens)
  expect_equal(nrow(test_index_netpen_rows), 5)
})
