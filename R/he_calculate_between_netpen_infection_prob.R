he_calculate_between_netpen_infection_prob <- function(
  netpen_to_netpen,
  infected_netpen_info
) {
  n_infectious <- sum(
    infected_netpen_info$n_subclinical +
      infected_netpen_info$n_clinical
  )
  n_not_immune <- sum(
    infected_netpen_info$n_total -
      (infected_netpen_info$n_recovered + infected_netpen_info$n_dead)
  )
  prevalence <- he_calculate_prevalence(n_infectious, n_not_immune)
  prob_inf <- 1 - (exp(-netpen_to_netpen * prevalence))
  prob_inf
}
