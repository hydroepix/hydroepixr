#' Add infected netpen to the infected farm info data frame
#'
#' @param inf_farm_info data frame of information on infected farms
#' @param farm_info data frame of information on netpens
#' @param new_inf_netpen_ids list of newly infected netpen ids
#' @param num_inf_animals_by_stage vector of numeric values indicating the
#'    of infected animals in each stage of infection (latent, clinical, and
#'    subclinical)
#' @param infection_origin source of the infection was can be "index",
#'    between-netpen", or "between-farm"
#' @param simulation_day day of simulation that infection is occurring
#'
#' @return data frame of information on infected farms, appended with rows to
#'    represent new infections
#' @export
#'
he_add_infected_netpen <-
  function(inf_farm_info,
           farm_info,
           new_inf_netpen_ids,
           num_inf_animals_by_stage,
           infection_origin,
           simulation_day
           ) {

    # Filter new infection ids to remove those already infected
    already_inf_ids <- new_inf_netpen_ids %in% inf_farm_info$netpen_id
    if (any(already_inf_ids)) {
      new_inf_netpen_ids <- new_inf_netpen_ids[!already_inf_ids]
      num_inf_animals_by_stage <-
        num_inf_animals_by_stage[!already_inf_ids,, drop = FALSE]
      infection_origin <- infection_origin[!already_inf_ids]
    }

    # If there are new infections left to add
    if (length(new_inf_netpen_ids) > 0) {
      new_inf_rows <-
        data.frame(
          simulation_day = simulation_day,
          netpen_id = new_inf_netpen_ids,
          farm_id = farm_info$farm_id[new_inf_netpen_ids],
          species_id = farm_info$species_id[new_inf_netpen_ids],
          within_netpen_transmission = farm_info$within_netpen_transmission[new_inf_netpen_ids],
          n_susceptible = farm_info$netpen_size[new_inf_netpen_ids] -
            rowSums(num_inf_animals_by_stage),
          num_inf_animals_by_stage,
          n_immune = 0,
          n_total = farm_info$netpen_size[new_inf_netpen_ids],
          infection_status = 1,
          infection_origin = infection_origin,
          day_infected = simulation_day,
          is_vaccinated = 0
        )
      # Assign maximum status value
      # new_inf_rows$status <-
      #   apply(new_inf_rows[, c("latent", "subclinical", "clinical"), drop = FALSE], 1, function(x)
      #     max(c("latent", "subclinical", "clinical")[x >= 1]))
      # Add newly infected rows into infected farm data

      # Row names needs to be false due to the match condition above that checks
      # for already infected netpens - it seems to convert row names to
      # character vectors instead of numeric values
      inf_farm_info <-
        rbind(inf_farm_info, new_inf_rows, make.row.names = FALSE)
    }
    inf_farm_info
  }
