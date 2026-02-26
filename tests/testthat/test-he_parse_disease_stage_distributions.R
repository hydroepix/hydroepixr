test_that("infection stage distributions are parsed correctly", {
  species_info <-
    readRDS(paste0(test_data_filepath, "/raw_species_info_bay_x.rds"))
  test_species_info_row <- species_info[1, ]
  parsed_species_info <-
    readRDS(paste0(test_data_filepath, "/parsed_species_info_bay_x.rds"))
  test_parsed_species_info_row <-
    he_parse_infection_stage_distributions(test_species_info_row)
  expect_equal(test_parsed_species_info_row, parsed_species_info[1, ])
})
