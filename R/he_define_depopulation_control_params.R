#' Define simulation variables related to depopulation control
#'
#' @param environment environment in which simulation variables are stored and managed
#' @param culling_capacity vector of integer values indicating the number of individuals which can be culled??
#' @param cull_types vector of indices to indicate which species can be culled??
#' @param cull_farm_if_cage_infection boolean indicating whether the entire farm should be culled if an infection is detected in a cage
#'
#' @return NA
#' @export
#'
he_define_depopulation_control_params <-
  function(environment,
           culling_capacity = c(20000),
           cull_types = c(1:18),
           cull_farm_if_cage_infection = FALSE # TODO: renaming of cullAll? better renaming?
           ) {
   environment$culling_capacity <- culling_capacity
   environment$cull_types <- cull_types
   environment$cull_farm_if_cage_infection <-
     cull_farm_if_cage_infection
   }
