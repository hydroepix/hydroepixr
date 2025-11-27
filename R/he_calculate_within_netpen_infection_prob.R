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
    prevalence <- he_calculate_prevalence(infected_netpen_info)
    within_netpen_infection_prob <- 1 - exp(-transmission_factor * prevalence)
    within_netpen_infection_prob
  }
