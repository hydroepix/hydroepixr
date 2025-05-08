#' Calculate probability of infection transmission within a netpen
#'
#' @param inf_farm_info data frame of infected farm information
#' @param vaccine_efficacy product of the manufacturer-reported vaccine efficacy
#'    and the population coverage of the vaccine
#'
#' @return probability of within-netpen infection transmission
#' @export
#'
he_calculate_within_netpen_infection_prob <-
  function(inf_farm_info,
           vaccine_efficacy) {
    effective_transmission <-
      inf_farm_info$within_netpen_transmission * (1 - vaccine_efficacy)
    prevalence <-
      (inf_farm_info$n_subclinical + inf_farm_info$n_clinical) /
      (inf_farm_info$n_total - inf_farm_info$n_immune)
    within_netpen_infection_prob <- 1 - exp(-effective_transmission * prevalence)
    within_netpen_infection_prob
  }
