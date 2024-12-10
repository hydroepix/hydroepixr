#' Initialize infection as a randomized number of random fish
#'
#' @param contact_farms which farms have had contact with the vector of infection?
#' @param prob_infection probability of infection of ...?
#' @param susceptibility susceptibility? to infection? logical yes/no?
#'
#' @return NA
#' @export
#'
he_initialize_infected_fish_stochastic <-
  function(contact_herds,
           prob_infection,
           susceptibility) {
    rbinom(length(contact_herds), 1, prob_infection)
  }
