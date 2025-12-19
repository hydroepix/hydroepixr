#' Calculate probability of infection transmission within a net pen
#'
#' @param infected_net_pen_info data frame of infected net pen information
#'
#' @return probability of transmission of infection between net pens
#' @export
#'
he_calculate_within_net_pen_infection_prob <-
  function(infected_net_pen_info) {
    transmission_factor <- infected_net_pen_info$within_net_pen_transmission
    n_infectious <- infected_net_pen_info$n_subclinical +
      infected_net_pen_info$n_clinical
    n_not_immune <- infected_net_pen_info$n_total -
      (infected_net_pen_info$n_recovered + infected_net_pen_info$n_dead)
    prevalence <- he_calculate_prevalence(n_infectious, n_not_immune)
    within_net_pen_infection_prob <- 1 - exp(-transmission_factor * prevalence)
    within_net_pen_infection_prob
  }
