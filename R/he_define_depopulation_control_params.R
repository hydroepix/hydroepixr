#' Define simulation variables related to depopulation control
#'
#' @param environment environment in which simulation variables are stored and
#'   managed
#' @param depop_capacity integer value indicating the number of individuals
#'   which can be depopulated per day
#' @param species_to_depop vector of indices to indicate which species can be depopulated??
#' @param depop_farm_if_netpen_infected boolean indicating whether the entire farm
#'   should be depopulated if an infection is detected in at least one netpen
#'
#' @return NA
#' @export
#'
he_define_depopulation_control_params <-
  function(environment,
           depop_capacity = 20000,
           species_to_depop = c(1:18), # possibly no longer used
           depop_farm_if_netpen_infected = FALSE) {
    environment$depop_capacity <- depop_capacity
    environment$species_to_depop <- species_to_depop
    environment$depop_farm_if_netpen_infected <-
      depop_farm_if_netpen_infected
  }
