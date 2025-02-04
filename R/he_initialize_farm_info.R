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
  if (any(sapply(initialized_farm_info$within_netpen_transmission, is.na)) |
      any(sapply(initialized_farm_info$rel_susceptibility, is.na))) {
    stop("Missing species info for species_id in farm_info.
         Please check files to ensure all species_id in the farm_info file have
         corresponding information in the species_info file.")
  }
  initialized_farm_info
}
