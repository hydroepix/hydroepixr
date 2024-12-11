#' Define simulation variables related to depopulation control
#'
#' @param environment environment in which simulation variables are stored and
#'   managed
#' @param culling_capacity vector of integer values indicating the number of
#'   individuals which can be culled on a given day
#' @param species_to_cull vector of indices to indicate which species can be culled??
#' @param cull_farm_if_cage_infected boolean indicating whether the entire farm
#'   should be culled if an infection is detected in a cage
#'
#' @return NA
#' @export
#'
he_define_depopulation_control_params <-
  function(environment,
           culling_capacity = c(20000),
           species_to_cull = c(1:18), # possibly no longer used
           cull_farm_if_cage_infected = FALSE) {
    environment$culling_capacity <- culling_capacity
    environment$species_to_cull <- species_to_cull
    environment$cull_farm_if_cage_infected <-
      cull_farm_if_cage_infected
  }
