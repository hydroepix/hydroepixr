library(here)
library(dplyr)

# Read in current test data
test_data_file_path <- here("inst/testdata")
# TODO:
test_data_file_name <- paste0(test_data_file_path, "/initialized_net_pen_info_bay_x.rds")

test_data <- readRDS(test_data_file_name)

# Update test data
# TODO:
test_data <- test_data %>% select(-is_vaccinated)

# Rewrite test data
saveRDS(test_data, test_data_file_name)
