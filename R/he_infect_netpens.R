#' Infect new netpens
#'
#' @param farm_info matrix of farm information
#' @param inf_farm_info matrix of information on infected farms
#' @param species_info matrix of species information
#' @param newly_infected_farm_ids farm identifiers which should have newly
#'    infected netpens
#' @param sim_day day timestep in the simulation
#' @param label ? contact method potentially?
#'
#' @return ?
#' @export
#'
he_infect_netpens <-
  function(farm_info,
           inf_farm_info,
           species_info,
           newly_infected_farm_ids,
           sim_day,
           label
  ) {
  # Identify valid netpens for infection within farms
  # TODO: WIP Why are we unlisting here?
  inf_netpen_ids <-
    unlist(sapply(
      farm_info,
      he_identify_netpens_for_infection,
      newly_infected_farm_ids
    ))
    # TODO: This seems to select a single netpen for infection from the valid
    # netpens identified - is that correct? We check for the length of the
    # selected netpens below on line 19
    if (length(inf_netpen_ids) > 1)  {
      inf_netpen_ids <- sample(inf_netpen_ids, size = 1)
    }

  # If at least one valid netpen for infection has been identified?
  if (length(inf_netpen_ids) > 0) {
    # Randomly select newly infected animals from the infected netpens
    # This should be a 3 column matrix for latent stage, subclinical stage,
    # and clinical stage
    new_inf_animals <- round(he_rpert(length(inf_netpen_ids), 1, 10, 100))
    all_new_infs <- c(all_new_infs, inf_netpen_ids)
    all_new_animals <- c(all_new_animals,
                         new_inf_animals[as.logical(new_inf_animals)])
    farm_info$status[inf_netpen_ids] <- 2
    farm_info$time_infected[inf_netpen_ids] <- sim_day
    farm_info$infection_mode[inf_netpen_ids] <- label
    # Replace this with a call to a new function, not a method
    he_add_infections_to_inf_farm_info(inf_farm_info,
                                       species_info,
                                       new_inf_farm_ids,
                                       num_inf_animals)
  }
}
