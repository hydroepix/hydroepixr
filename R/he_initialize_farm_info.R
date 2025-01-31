#' Initialize farm information from input files and parameters
#'
#' @param farm_info data frame storing information on farms
#' @param species_info data frame storing information on species
#'
#' @return data frame of farm information initialized with necessary columns
#'    and necessary species information for the entire model run
#' @export
#'
#' @examples
he_initialize_farm_info <- function(farm_info, species_info) {
  num_farms <- length(farm_info$farm_id)
  farm_info$susceptible <-
    purrr::map(farm_info$species_id,
               ~ (farm_species_id) species_info[farm_species_id]$rel_susceptibility)
  farm_info$k <- rep(NA, num_farms)
  farm_info$depop_eligible <- rep(TRUE, environment$num_farms)
  # TODO: Check that this isn't a logical argument instead of a string?
  # Check depop-eligibility by species
  if (!identical(environment$species_to_depop, "all")) {
    environment$farm_info$depop_eligible <-
      environment$farm_info$species %in% environment$species_to_depop
  }

  # Set initial farm infection status to 1 for every farm unless values
  # are taken from the file
  if (ignore_status) {
    environment$farm_info$initial_status <- rep(1, environment$num_farms)
    environment$farm_info$initial_time_infected <- rep(Inf, environment$num_farms)
  } else {
    environment$farm_info$initial_status <- environment$farm_info$status
    environment$farm_info$initial_time_infected <- environment$farm_info$time_infected
  }

  farm_info
}
