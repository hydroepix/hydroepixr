test_that("farm info is initialized correctly", {
  farm_info <- readRDS(paste0(test_data_filepath, "/raw_farm_info_bay_x.rds"))
  species_info <- readRDS(paste0(test_data_filepath, "/raw_species_info_bay_x.rds"))
  initialized_farm_info <- he_initialize_farm_info(farm_info, species_info)
})
