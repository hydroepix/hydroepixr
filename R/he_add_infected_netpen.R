#' Add infected netpen to the infected farm info data frame
#'
#' @param inf_farm_info data frame of information on infected farms
#' @param farm_info data frame of information on netpens
#' @param new_inf_netpen_ids list of newly infected netpen ids
#' @param num_inf_animals_by_stage vector of numeric values indicating the
#'    of infected animals in each stage of infection (latent, clinical, and
#'    subclinical)
#' @param type_of_contact type of contact that resulted in the infection, can be
#'    either direct (within/between netpens) or indirect (between farms)
#' @param sim_timestep timestep or day of simulation that infection is occurring
#'
#' @return data frame of information on infected farms, appended with rows to
#'    represent new infections
#' @export
#'
# TODO: Handle infection status variable?
he_add_infected_netpen <-
  function(inf_farm_info,
           farm_info,
           new_inf_netpen_ids,
           num_inf_animals_by_stage,
           type_of_contact,
           sim_timestep
           ) {

    # Filter new infection ids to remove those already infected
    already_inf_ids <- new_inf_netpen_ids %in% inf_farm_info$netpen_id
    if (any(already_inf_ids)) {
      new_inf_netpen_ids <- new_inf_netpen_ids[!already_inf_ids]
      num_inf_animals_by_stage <-
        num_inf_animals_by_stage[!already_inf_ids,, drop = FALSE]
      type_of_contact <- type_of_contact[!already_inf_ids]
    }

    # If there are new infections left to add
    if (length(new_inf_netpen_ids) > 0) {
      new_inf_rows <-
        data.frame(
          netpen_id = new_inf_netpen_ids,
          farm_id = farm_info$farm_id[new_inf_netpen_ids],
          species_id = farm_info$species_id[new_inf_netpen_ids],
          within_netpen_transmission = farm_info$within_netpen_transmission[new_inf_netpen_ids],
          # TODO: Clarify difference between susceptibility
          # and susceptible count
          susceptible = farm_info$netpen_size[new_inf_netpen_ids] -
            rowSums(num_inf_animals_by_stage),
          num_inf_animals_by_stage,
          immune = 0,
          total = farm_info$netpen_size[new_inf_netpen_ids],
          infection_status = 1,
          latent_duration = 0,
          subclinical_duration = 0,
          clinical_time = Inf,
          time_of_diagnosis = Inf,
          diagnosed = 0,
          infected_by_direct_contact = type_of_contact,
          time_infected = sim_timestep,
          vaccinated = 0 # shouldn't this come from somewhere
          #instead of a default of 0?,
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
