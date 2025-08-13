library(dplyr)

filename <- "inst/testdata/infected_netpen_info_dummy_data_with_infection.rds"

infected_netpen_info <- readRDS(filename)

infected_netpen_info <- infected_netpen_info %>%
  rename(n_recovered = n_immune) %>%
  mutate(n_dead = c(0, 0, 0), .after = "n_recovered")

saveRDS(infected_netpen_info, filename)

