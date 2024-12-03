he_initialize_infected_fish_stochastic <-
  function(contact_herds,
           prob_infection,
           susceptibility) {
    rbinom(length(contact_herds), 1, prob_infection)
  }
