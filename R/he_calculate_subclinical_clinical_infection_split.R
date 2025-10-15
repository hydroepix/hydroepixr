he_calculate_subclinical_clinical_infection_split <-
  function(n_transitioning,
           clinically_infected_prop) {
    n_newly_clinical <- ceiling(n_transitioning * clinically_infected_prop)
    n_newly_subclinical <- n_transitioning - n_newly_clinical
    c(n_newly_subclinical, n_newly_clinical)
  }
