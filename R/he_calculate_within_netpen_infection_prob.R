#' Calculate probability of infection transmission within a netpen
#'
#' @param infected_netpen_info data frame of infected netpen information
#'
#' @return probability of within-netpen infection transmission
#' @export
#'
he_calculate_within_netpen_infection_prob <-
  function(infected_netpen_info) {
    transmission_factor <- infected_netpen_info$within_netpen_transmission
    n_infectious <- infected_netpen_info$n_subclinical +
      infected_netpen_info$n_clinical
    n_not_immune <- infected_netpen_info$n_total -
      (infected_netpen_info$n_recovered + infected_netpen_info$n_dead)
    prevalence <- he_calculate_prevalence(n_infectious, n_not_immune)
    within_netpen_infection_prob <- 1 - exp(-transmission_factor * prevalence)
    within_netpen_infection_prob
  }
