test_that("net pen info values are initialized correctly", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x.rds"
  ))
  species_info <- readRDS(paste0(
    test_data_filepath,
    "/parsed_species_info_bay_x.rds"
  ))
  initialized_net_pen_info <- he_initialize_net_pen_info(
    net_pen_info,
    species_info
  )
  expect_true(all(
    initialized_net_pen_info$rel_susceptibility == 1
  ))
  # Confirm within net pen transmission distribution values are within the
  # expected range
  expect_true(all(initialized_net_pen_info$within_net_pen_transmission >= 0.14))
  expect_true(all(initialized_net_pen_info$within_net_pen_transmission <= 0.8))
})

test_that("net pen info species with missing species info causes an error", {
  net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/raw_net_pen_info_bay_x_species_id_mismatch.rds"
  ))
  species_info <- readRDS(paste0(
    test_data_filepath,
    "/parsed_species_info_bay_x.rds"
  ))
  expect_error(
    he_initialize_net_pen_info(net_pen_info, species_info),
    regex = "Missing species info for a species in net_pen_info.
         Please check files to ensure all species_id in the net_pen_info file have
         corresponding information in the species_info file."
  )
})
