#' Initialize net pen information from input files and parameters
#'
#' @param net_pen_info data frame storing information on net pens
#' @param species_info data frame storing information on species
#'
#' @return data frame of net pen information initialized with necessary columns
#'    and necessary species information for the entire model run
#'
he_initialize_net_pen_info <- function(net_pen_info, species_info) {
  species_info_subset <-
    subset(
      species_info,
      subset = TRUE, # this appears to be necessary for a df for some reason
      select = c(
        "species_id",
        "within_net_pen_transmission_min",
        "within_net_pen_transmission_mode",
        "within_net_pen_transmission_max",
        "rel_susceptibility"
      )
    )

  # left outer join
  initialized_net_pen_info <- merge(
    net_pen_info,
    species_info_subset,
    all.x = TRUE
  )
  missing_species_info <- unlist(purrr::map(
    initialized_net_pen_info[c(
      "within_net_pen_transmission_min",
      "within_net_pen_transmission_mode",
      "within_net_pen_transmission_max",
      "rel_susceptibility"
    )],
    \(x) any(is.na(x))
  ))

  # Check for any species_id in net_pen_info without corresponding details in
  # species_info
  if (any(missing_species_info)) {
    stop(
      "Missing species info for a species in net_pen_info.
         Please check files to ensure all species_id in the net_pen_info file have
         corresponding information in the species_info file."
    )
  }

  # Calculate random variates for within net pen transmission
  # from triangular distribution
  initialized_net_pen_info$within_net_pen_transmission <- unlist(purrr::pmap(
    list(
      1,
      initialized_net_pen_info$within_net_pen_transmission_min,
      initialized_net_pen_info$within_net_pen_transmission_mode,
      initialized_net_pen_info$within_net_pen_transmission_max
    ),
    he_rpert
  ))
  # Return net pen info without intermediate columns
  return(subset(
    initialized_net_pen_info,
    select = -c(
      within_net_pen_transmission_min,
      within_net_pen_transmission_mode,
      within_net_pen_transmission_max
    )
  ))
}
