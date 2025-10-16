he_calculate_subclinical_clinical_infection_split <-
  function(n_transitioning,
           clinically_infected_prop) {
    n_newly_clinical <- as.matrix(ceiling(n_transitioning * clinically_infected_prop))
    n_newly_subclinical <- as.matrix(n_transitioning - n_newly_clinical)
    cbind(n_newly_subclinical, n_newly_clinical, deparse.level = 0)
  }
