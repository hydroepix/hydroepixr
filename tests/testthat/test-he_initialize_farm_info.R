test_that("farm info values are initialized correctly", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  species_info <- readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  initialized_farm_info <- he_initialize_farm_info(farm_info, species_info)
  expect_true(all(
    initialized_farm_info$rel_susceptibility == 1))
  # expect_true(all(
  #   initialized_farm_info$within_netpen_transmission == "rpert(n,0.14,0.4,0.8)"))
})

test_that("farm info species with missing species info causes an error", {
  farm_info <- readRDS(paste0(test_data_filepath,
                              "/raw_farm_info_bay_x_species_id_mismatch.rds"))
  species_info <- readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  expect_error(he_initialize_farm_info(farm_info, species_info),
               regex = "Missing species info for a species in farm_info.
         Please check files to ensure all species_id in the farm_info file have
         corresponding information in the species_info file.")
})
