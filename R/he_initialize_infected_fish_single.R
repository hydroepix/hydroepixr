he_initialize_infected_fish_single <-
  function(contact_herds,
           prob_infection,
           susceptibility) {
    rbinom(length(contact_herds),
           farm_info$susceptibility[contact_herds],
           1 - (1 - prob_infection) ^ (1 / susceptibility[contact_herds]))
  }
