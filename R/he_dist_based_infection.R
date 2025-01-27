# Determines infection based on distance to infected farms for a given day
he_dist_based_infection <- function(farm_info,
                                    g_time,
                                    dist_mat,
                                    farm_to_farm,
                                    vaccine_efficacy,
                                    dist_mat_type = "seaway distance",
                                    label = 1,
                                    t_start = 0,
                                    t_end = Inf) {
  if (g_time > t_start & g_time < t_end) {
    all_new_infections <- NULL
    all_new_animals <- NULL

    # Distance-based spread from other farms
    farm_ids <- unique(farm_info$farm_id)
    # Retrieve susceptibility of each farm (mean)
    farm_susceptibility <- tapply(farm_info$rel_susceptibility,
                                  farm_info$farm_id,
                                  mean)
    # Retrieve infectiousness of each farm (max)
    farm_infectiousness <- as.logical(tapply(farm_info$infectiousness,
                                             farm_info$farm_id,
                                             max))
    ## This needs to be checked in relation to culling capacity (????)
    # TODO: Why does an NA infectiousness value get converted to true?
    farm_infectiousness[is.na(farm_infectiousness)] <- TRUE
    # Retrieve active status of farms
    farm_active <- tapply(farm_info$farm_active,
                          farm_info$farm_id,
                          any)
    # Calculate infection probability vector for each farm using the specified
    # method
    inf_prob_vec <- he_calculate_inf_prob_vec(
      he_calculate_inf_prob_matrix(
        dist_mat,
        farm_ids,
        farm_to_farm,
        vaccine_efficacy,
        farm_active,
        farm_susceptibility,
        farm_infectiousness,
        dist_mat_type,
      )
    )

    # Apply infection probability vector to determine newly infected farms
    newly_infected_farms <- rbinom(length(farm_ids), 1, inf_prob_vec)
    newly_infected_farm_ids <- farm_ids[newly_infected_farms]

    # Infect cages in any farms selected to be newly infected
    # TODO: What is g_time? Why does it need to be greater than 60?
    if (length(newly_infected_farms > 0) & g_time > 60) {
      newly_infected_cages <- he_infect_cages()
    }

  }
}
