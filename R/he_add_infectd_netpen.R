#' Add infected netpen to the infected farm info data frame
#'
#' @param inf_farm_info data frame of information on infected farms
#' @param species_info data frame of information on species
#' @param netpen_ids_to_infect list of newly infected netpen ids
#' @param num_inf_animals_by_stage vector of three numeric values indicating the
#'    of infected animals in each stage of infection (latent, clinical, and
#'    subclinical)
#' @param type_of_contact type of contact that resulted in the infection, can be
#'    either direct (within/between netpens) or indirect (between farms)
#'
#' @return data frame of information on infected farms, appended with rows to
#'    represent new infections
#' @export
#'
he_add_infected_netpen <-
  function(inf_farm_info,
           species_info,
           netpen_ids_to_infect,
           num_inf_animals_by_stage,
           type_of_contact
           ) {

    #   # TODO: infect index farm with number of fish according to type of
    #   # initial infection
    #   if (index_direct) {
    #     # TODO: construct an infected farm
    #     # aInfHerd$addInf(indexHerd,
    #     #                 matrix(c(0, 1, 0),
    #     #                        byrow = TRUE,
    #     #                        ncol = 3,
    #     #                        nrow = length(indexHerd)),
    #     #                 1)
    #   } else {
    #     # TODO: construct an infected farm
    #     # aInfHerd$addInf(indexHerd,
    #     #                 matrix(c(1, 0, 0),
    #     #                        byrow = TRUE,
    #     #                        ncol = 3,
    #     #                        nrow = length(indexHerd)),
    #     #                 0)
    #   }

    # Filter new infection ids to remove those already infected
    # TODO: Double check column indexing
    already_inf_ids <- new_inf_farm_ids %in% inf_farm_info$farm_id
    if (any(already_inf_ids)) {
      new_inf_farm_ids <- new_inf_farm_ids[!already_inf_ids]
      num_inf_animals <- num_inf_animals[!already_inf_ids,, drop = FALSE]
      #DC <- DC[already_inf_ids]
    }

    # If there are new infections left to add
    if (length(new_inf_farm_ids) > 0) {
      new_inf_rows <-
        cbind(
          new_inf_farm_ids, # farm_id
          farm_info$species_id[new_inf_farm_ids], # species_id
          farm_info$susceptibility[new_inf_farm_ids], # susceptible
          0, # latent
          0, # subclinical
          0, # clinical
          0, # immune
          farm_info$susceptibility[new_inf_farm_ids], # total number of still-susceptible animals
          # i.e. not immune and not infected
          1, # infection_status?
          #farm_info$p[new_inf_farm_ids], #p?
          0, # latent_duration
          0, # subclinical_duration
          Inf, # clinical_time
          Inf, # time of diagnosis
          0, # diagnosed
          0, # infected_by_direct_contact
          sim_timestep, # time_infected
          0, # vaccinated - shouldn't this come from somewhere
          #instead of a default of 0?
          #0 # "TLastAnCli19"
        )
      new_inf_rows[, 1:4] <-
        new_inf_rows[, 1:4] + cbind(-rowSums(num_inf_animals), num_inf_animals)
      # Assign maximum status value
      new_inf_rows[, 7] <-
        apply(new_inf_rows[, 1:4, drop = FALSE], 1, function(x)
          max((1:4)[x >= 1]))

      # Updating diagnosis (time?) if initial status is 4 (Clinical) ??)
      # TODO: Why are we calling the browser?? This can likely be removed
      #if (is.na(new_inf_rows[1,1])) browser()
      if (any(new_inf_rows_index <- (new_inf_rows[,7] == 4))) {
        new_inf_rows[new_inf_rows_index, 12] <- sim_day
      }

      # Add generated newly infected rows into infected farm data
      inf_farm_info$farms <- rbind(inf_farm_info$farms, new_inf_rows)

      # Distribute the animals on number of days as latent, subclinical, or clinical stages
      inf_farm_info$latent <- rbind(
        inf_farm_info$latent,
        he_apply_infection_stage_distribution(
          new_inf_rows,
          species_info,
          stage = "latent")
      )
      inf_farm_info$subclinical <- rbind(
        inf_farm_info$subclinical,
        he_apply_infection_stage_distribution(
          new_inf_rows,
          species_info,
          stage = "subclinical")
      )
      inf_farm_info$clinical <- rbind(
        inf_farm_info$clinical,
        he_apply_infection_stage_distribution(
          new_inf_rows,
          species_info,
          stage = "clinical")
      )

      # Add rows to the dead animal matrices in the infected farm info
      inf_farm_info$dead_animal_matrix <-
        rbind(inf_farm_info$dead_animal_matrix,
              t(sapply(new_inf_farm_ids,
                       function(x) rep(0, days_dead_infectious))))
      inf_farm_info$dead_animal_matrix_sur <-
        rbind(inf_farm_info$dead_animal_matrix_sur,
              t(sapply(new_inf_farm_ids,
                       function(x) rep(0, past_days_for_dead_animal_surveillance))))
    }
    inf_farm_info
  }
