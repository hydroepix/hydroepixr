test_that("invalid net pen IDs produces error", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x.rds"
  ))
  expect_error(
    he_select_index_net_pens(
      net_pen_info,
      net_pen_ids = c(56, 57, 58, 59, 60, 61)
    ),
    regex = "Invalid net pen ID identified. Please confirm all provided net pen
             IDs exist in the net pen info file."
  )
})

test_that("lack of net pen or net pen ID produces error", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x.rds"
  ))
  expect_error(
    he_select_index_net_pens(net_pen_info),
    regex = "Either a specific net pen ID or a farm ID must be provided in order
             to select an index net pen."
  )
})

test_that("invalid farm ID produces error", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x.rds"
  ))
  expect_error(
    he_select_index_net_pens(net_pen_info, farm_id = 4),
    regex = "Farm ID provided not found in net pen info. Please confirm the
             provided farm ID exists in the net pen info file."
  )
})

test_that("providing both net pen ID and farm ID produces error", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x.rds"
  ))
  expect_error(
    he_select_index_net_pens(
      net_pen_info,
      net_pen_ids = c(1, 2, 3, 4, 5),
      farm_id = 3
    ),
    regex = "Either net pen ID or farm ID should be provided, not both."
  )
})

test_that("randomized index net pen selection from farm ID works with default
          of 1 net pen", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x.rds"
  ))
  test_index_net_pens <- he_select_index_net_pens(net_pen_info, farm_id = 3)
  # Check the appropriate number of net pens have been selected
  expect_equal(length(test_index_net_pens), 1)
  # Check that the selected net pen exists in the data frame
  test_index_net_pen_row <- subset(
    net_pen_info,
    net_pen_info$net_pen_id %in% test_index_net_pens
  )
  expect_equal(nrow(test_index_net_pen_row), 1)
})

test_that("randomixed index net pen selection from farm ID works with specified
          number of net pens", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x.rds"
  ))
  test_index_net_pens <- he_select_index_net_pens(
    net_pen_info,
    farm_id = 2,
    n_infected_net_pens = 3
  )
  # Check the appropriate number of net pens have been selected
  expect_equal(length(test_index_net_pens), 3)
  # Check that the selected net pens exist in the data frame
  test_index_net_pen_rows <- subset(
    net_pen_info,
    net_pen_info$net_pen_id %in% test_index_net_pens
  )
  expect_equal(nrow(test_index_net_pen_rows), 3)
  # Check that the selected net pens exist as part of the specified farm
  expect_true(all(test_index_net_pen_rows$farm_id == 2))
})

test_that("index net pen selection from net pen ID works", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x.rds"
  ))
  test_index_net_pens <- he_select_index_net_pens(
    net_pen_info,
    net_pen_ids = c(45, 46, 47, 48, 49)
  )
  # Check the appropriate number of net pens have been selected
  expect_equal(length(test_index_net_pens), 5)
  # Check that the selected net pens exist in the data frame
  test_index_net_pen_rows <- subset(
    net_pen_info,
    net_pen_info$net_pen_id %in% test_index_net_pens
  )
  expect_equal(nrow(test_index_net_pen_rows), 5)
})
