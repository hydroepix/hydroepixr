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
           select = c(
             "species_id",
             "within_netpen_transmission_min",
             "within_netpen_transmission_mode",
             "within_netpen_transmission_max",
             "rel_susceptibility"
           ))

  # left outer join
  initialized_farm_info <- merge(farm_info,
                                 species_info_subset,
                                 all.x = TRUE)
  missing_species_info <- unlist(purrr::map(
    initialized_farm_info[c(
      "within_netpen_transmission_min",
      "within_netpen_transmission_mode",
      "within_netpen_transmission_max",
      "rel_susceptibility"
    )],
    \(x) any(is.na(x)))
    )

  # Check for any species_id in farm_info without corresponding details in
  # species_info
  if (any(missing_species_info)) {
    stop("Missing species info for a species in farm_info.
         Please check files to ensure all species_id in the farm_info file have
         corresponding information in the species_info file.")
  }

  # Calculate random variates for within netpen transmission
  # from triangular distribution
  initialized_farm_info$within_netpen_transmission <- purrr::pmap(
    list(
      1,
      initialized_farm_info$within_netpen_transmission_min,
      initialized_farm_info$within_netpen_transmission_mode,
      initialized_farm_info$within_netpen_transmission_max
    ),
    he_rpert
  )
  # Remove intermediate columns
  subset(initialized_farm_info,
         select = -c(within_netpen_transmission_min,
                     within_netpen_transmission_mode,
                     within_netpen_transmission_max))
}
