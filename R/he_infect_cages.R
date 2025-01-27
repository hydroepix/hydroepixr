he_infect_cages <- function(farm_info, newly_infected_farm_ids, g_time, label) {
  # Identify valid netpens for infection within farms
  # TODO: WIP Why are we unlisting here?
  new_inf_netpens <-
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
    inf_netpen_ids

  # If at least one valid netpen for infection has been identified?
  if (length(new_inf_netpens) > 0) {
    # Randomly select newly infected animals from the infected netpens
    # This should be a 3 column matrix for latent stage, subclinical stage,
    # and clinical stage
    new_inf_animals <- round(he_rpert(length(inf_netpen_ids), 1, 10, 100))
    all_new_infs <- c(all_new_infs, inf_netpen_ids)
    all_new_animals <- c(all_new_animals,
                         new_inf_animals[as.logical(new_inf_animals)])
    farm_info$status[inf_netpen_ids] <- 2
    farm_info$time_infected[inf_netpen_ids] <- g_time
    farm_info$infection_mode[inf_netpen_ids] <- label
    # TODO: Replace this with a call to a new function, not a method
    aInfHerd$addInf(AllNewInfs, cbind(AllNewAnimals, 0, 0), 0)
  }
}
