library(dplyr)

infected_netpen_info <-
  readRDS("inst/testdata/infected_netpen_info_bay_x_with_multi_farm_infection.rds")

infected_netpen_info <- infected_netpen_info %>%
  rename(n_susceptible = susceptible,
         n_latent = latent,
         n_subclinical = subclinical,
         n_clinical = clinical,
         n_immune = immune,
         n_total = total,
         day_infected = time_infected,
         is_vaccinated = vaccinated) %>%
  mutate(infection_origin = c("index")) %>%
  relocate(infection_origin, .after = infection_status) %>%
  select(
    -c(
      "latent_duration",
      "subclinical_duration",
      "clinical_time",
      "time_of_diagnosis",
      "diagnosed",
      "infected_by_direct_contact"
    )
  )

infected_netpen_info <- infected_netpen_info %>% mutate(infection_status = c("latent", "subclinical"))

saveRDS(infected_netpen_info,
        "inst/testdata/infected_netpen_info_bay_x_with_multi_farm_infection")

