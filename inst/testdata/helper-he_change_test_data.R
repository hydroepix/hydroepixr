library(here)
library(dplyr)


test_data_file_path <- here("inst/testdata")
# TODO:
test_data_file_name <- paste0(test_data_file_path, "/infected_net_pen_info_dummy_data_with_infection.rds")


test_data <- readRDS()


test_data <- test_data %>%
  # TODO:
  rename(n_recovered = n_immune) %>%
  mutate(n_dead = c(0, 0, 0), .after = "n_recovered")

saveRDS(test_data, test_data_file_name)
