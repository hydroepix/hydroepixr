#' Calculate probability of infection transmission between net pens within a farm
#' @param net_pen_to_net_pen daily probability for infection between net pens
#' transmission within a farm
#' @param infected_net_pen_info data frame of infected net pen information
#'
#' @return probability of transmission of infection between net pens
#' @export
#'
he_calculate_between_net_pen_infection_prob <- function(
  net_pen_to_net_pen,
  infected_net_pen_info
) {
  n_infectious <- sum(
    infected_net_pen_info$n_subclinical +
      infected_net_pen_info$n_clinical
  )
  n_not_immune <- sum(
    infected_net_pen_info$n_total -
      (infected_net_pen_info$n_recovered + infected_net_pen_info$n_dead)
  )
  prevalence <- he_calculate_prevalence(n_infectious, n_not_immune)
  prob_inf <- 1 - (exp(-net_pen_to_net_pen * prevalence))
  prob_inf
}
