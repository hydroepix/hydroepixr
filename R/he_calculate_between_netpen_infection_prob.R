he_calculate_between_netpen_infection_prob <- function(
  netpen_to_netpen,
  infected_netpen_info
) {
  prevalence <- he_calculate_prevalence(infected_netpen_info)
  prob_inf <- 1 - (exp(-netpen_to_netpen * prevalence))
  prob_inf
}
