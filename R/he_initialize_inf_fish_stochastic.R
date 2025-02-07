#' Initialize infection as a randomized number of random fish
#'
#' @param contact_farms which farms have had contact with the vector of infection?
#' @param prob_infection probability of infection of ...?
#' @param susceptibility susceptibility? to infection? logical yes/no?
#'
#' @return NA
#' @export
#' @importFrom stats rbinom
#'
he_initialize_infected_fish_stochastic <-
  function(contact_farms,
           prob_infection,
           susceptibility) {
    # TODO: Clarify how initially infected fish are calculated
    # Existing code contained:
    rbinom(length(contact_herds), 1, prob_infection)
    # IG said initially infected fish are modelled in BCSpread() as:
    #rpert(1, 10, 100)
  }
