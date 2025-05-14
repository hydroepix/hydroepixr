#' Initialize netpen information from input files and parameters
#'
#' @param netpen_info data frame storing information on netpens
#' @param species_info data frame storing information on species
#'
#' @return data frame of netpen information initialized with necessary columns
#'    and necessary species information for the entire model run
#' @export
#'
he_initialize_netpen_info <- function(netpen_info, species_info) {
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
  initialized_netpen_info <- merge(netpen_info,
                                 species_info_subset,
                                 all.x = TRUE)
  missing_species_info <- unlist(purrr::map(
    initialized_netpen_info[c(
      "within_netpen_transmission_min",
      "within_netpen_transmission_mode",
      "within_netpen_transmission_max",
      "rel_susceptibility"
    )],
    \(x) any(is.na(x)))
    )

  # Check for any species_id in netpen_info without corresponding details in
  # species_info
  if (any(missing_species_info)) {
    stop("Missing species info for a species in netpen_info.
         Please check files to ensure all species_id in the netpen_info file have
         corresponding information in the species_info file.")
  }

  # Calculate random variates for within netpen transmission
  # from triangular distribution
  initialized_netpen_info$within_netpen_transmission <- unlist(purrr::pmap(
    list(
      1,
      initialized_netpen_info$within_netpen_transmission_min,
      initialized_netpen_info$within_netpen_transmission_mode,
      initialized_netpen_info$within_netpen_transmission_max
    ),
    he_rpert
  ))
  # Remove intermediate columns
  subset(initialized_netpen_info,
         select = -c(within_netpen_transmission_min,
                     within_netpen_transmission_mode,
                     within_netpen_transmission_max))
}
