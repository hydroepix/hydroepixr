#' Initialize infection as a single random fish
#'
#' @param contact_farms which farms have had contact with the vector of infection?
#' @param prob_infection probability of infection of ...?
#' @param susceptibility susceptibility? to infection? logical yes/no?
#'
#' @return ?
#' @export
#' @importFrom stats rbinom
#'
he_initialize_infected_fish_single <-
  function(contact_farms,
           prob_infection,
           susceptibility) {
    rbinom(length(contact_herds),
           farm_info$susceptibility[contact_herds],
           1 - (1 - prob_infection) ^ (1 / susceptibility[contact_herds]))
  }
