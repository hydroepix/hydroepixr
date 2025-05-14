#' Calculate probability of infection transmission within a netpen
#'
#' @param infected_netpen_info data frame of infected netpen information
#' @param vaccine_efficacy product of the manufacturer-reported vaccine efficacy
#'    and the population coverage of the vaccine
#'
#' @return probability of within-netpen infection transmission
#' @export
#'
he_calculate_within_netpen_infection_prob <-
  function(infected_netpen_info,
           vaccine_efficacy) {
    effective_transmission <-
      infected_netpen_info$within_netpen_transmission * (1 - vaccine_efficacy)
    prevalence <-
      (infected_netpen_info$n_subclinical + infected_netpen_info$n_clinical) /
      (infected_netpen_info$n_total - infected_netpen_info$n_immune)
    within_netpen_infection_prob <- 1 - exp(-effective_transmission * prevalence)
    within_netpen_infection_prob
  }
