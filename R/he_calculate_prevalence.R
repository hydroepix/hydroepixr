he_calculate_prevalence <- function(infected_netpen_info) {
  n_infectious <- infected_netpen_info$n_subclinical +
    infected_netpen_info$n_clinical
  n_susceptible_or_infected <- infected_netpen_info$n_total -
    (infected_netpen_info$n_recovered + infected_netpen_info$n_dead)
  prevalence <- n_infectious / n_susceptible_or_infected
  prevalence[is.nan(prevalence)] <- 0
  prevalence
}
