#' Add infected netpen to infected netpen tracking
#'
#' @param infected_netpen_info data frame of information on infected netpens
#' @param netpen_info data frame of information on netpens
#' @param new_infected_netpens list of newly infected netpen ids
#' @param n_infected_animals_by_stage vector of numeric values indicating the
#'    of infected animals in each stage of infection (latent, clinical, and
#'    subclinical)
#' @param infection_status overall infection status of the netpen, which is the
#'    latest disease stage found in the netpen
#' @param infection_origin source of the infection was can be "index",
#'    between-netpen", or "between-farm"
#' @param simulation_day day of simulation that infection is occurring
#'
#' @return data frame of information on infected netpens, appended with rows to
#'    for new infections
#' @export
#'
he_add_infected_netpen <-
  function(infected_netpen_info,
           netpen_info,
           new_infected_netpens,
           n_infected_animals_by_stage,
           infection_status,
           infection_origin,
           simulation_day
           ) {

    # Filter new infection ids to remove those already infected
    already_infected_netpens <- new_infected_netpens %in% infected_netpen_info$netpen_id
    if (any(already_infected_netpens)) {
      new_infected_netpens <- new_infected_netpens[!already_infected_netpens]
      n_infected_animals_by_stage <-
        n_infected_animals_by_stage[!already_infected_netpens,, drop = FALSE]
      infection_status <- infection_status[!already_infected_netpens]
      infection_origin <- infection_origin[!already_infected_netpens]
    }

    # If there are new infections left to add
    if (length(new_infected_netpens) > 0) {
      new_infected_netpen_rows <-
        data.frame(
          simulation_day = simulation_day,
          netpen_id = new_infected_netpens,
          farm_id = netpen_info$farm_id[new_infected_netpens],
          species_id = netpen_info$species_id[new_infected_netpens],
          within_netpen_transmission = netpen_info$within_netpen_transmission[new_infected_netpens],
          n_susceptible = netpen_info$netpen_size[new_infected_netpens] -
            rowSums(n_infected_animals_by_stage),
          n_infected_animals_by_stage,
          n_immune = 0,
          n_total = netpen_info$netpen_size[new_infected_netpens],
          infection_status = infection_status,
          infection_origin = infection_origin,
          day_infected = simulation_day,
          is_vaccinated = 0
        )
      # Assign maximum status value
      # new_infected_netpen_rows$status <-
      #   apply(new_infected_netpen_rows[, c("latent", "subclinical", "clinical"), drop = FALSE], 1, function(x)
      #     max(c("latent", "subclinical", "clinical")[x >= 1]))
      # Add newly infected rows into infected netpen data

      # Row names needs to be false due to the match condition above that checks
      # for already infected netpens - it seems to convert row names to
      # character vectors instead of numeric values
      infected_netpen_info <-
        rbind(infected_netpen_info, new_infected_netpen_rows, make.row.names = FALSE)
    }
    infected_netpen_info
  }
