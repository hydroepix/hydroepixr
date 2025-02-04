#' Initialize farm information from input files and parameters
#'
#' @param farm_info data frame storing information on farms
#' @param species_info data frame storing information on species
#'
#' @return data frame of farm information initialized with necessary columns
#'    and necessary species information for the entire model run
#' @export
#'
he_initialize_farm_info <- function(farm_info, species_info) {
  species_info_subset <-
    subset(species_info,
           subset = TRUE, # this appears to be necessary for a df for some reason
           select = c(species_id, within_netpen_transmission, rel_susceptibility))
  # left outer join
  initialized_farm_info <- merge(farm_info,
                                 species_info_subset,
                                 all.x = TRUE)
  # TODO: check for farm_info rows without matches
  initialized_farm_info
}
