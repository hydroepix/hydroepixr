# Determines infection based on distance to infected farms for a given day
he_dist_based_infection <- function(farm_info,
                                    g_time,
                                    inf_prob_matrix,
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

  }
}
