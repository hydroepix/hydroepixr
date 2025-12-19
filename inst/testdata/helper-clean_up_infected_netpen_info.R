library(dplyr)

filename <- "inst/testdata/infected_net_pen_info_dummy_data_with_infection.rds"

infected_net_pen_info <- readRDS(filename)

infected_net_pen_info <- infected_net_pen_info %>%
  rename(n_recovered = n_immune) %>%
  mutate(n_dead = c(0, 0, 0), .after = "n_recovered")

saveRDS(infected_net_pen_info, filename)
