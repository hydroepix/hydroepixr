#' Determines infection spread between farms for a given day?
#'
#' @param farm_info matrix of farm and netpen information
#' @param sim_day day timestep in the simulation
#' @param connectivity_matrix connectivity matrix in the form of a seaway
#'    distance matrix (in kilometers)or a hydroconnectivity matrix
#' @param farm_to_farm scaling parameter for between-farm infection transmission
#' @param vaccine_efficacy product of the manufacturer-reported vaccine efficacy
#'    and the population coverage of the vaccine
#' @param connectivity_matrix_type type of connectivity matrix provided, either
#'    distance matrix or hydroconnectivity matrix
#' @param label ? seems to apply to infection mode somehow? (direct or indirect)?
#' @param t_start threshold of earliest possible infection spread
#' @param t_end threshold for latest possible infection spread
#'
#' @return ?
#' @export
#' @importFrom stats rbinom
#'
he_dist_based_infection <- function(farm_info,
                                    sim_day,
                                    connectivity_matrix,
                                    farm_to_farm,
                                    vaccine_efficacy,
                                    connectivity_matrix_type = "distance",
                                    label = 1,
                                    t_start = 0,
                                    t_end = Inf) {
  if (sim_day > t_start & sim_day < t_end) {
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
        connectivity_matrix,
        farm_ids,
        farm_to_farm,
        vaccine_efficacy,
        farm_active,
        farm_susceptibility,
        farm_infectiousness,
        connectivity_matrix_type
      )
    )

    # Apply infection probability vector to determine newly infected farms
    newly_infected_farms <- rbinom(length(farm_ids), 1, inf_prob_vec)
    newly_infected_farm_ids <- farm_ids[newly_infected_farms]

    # Infect netpens in any farms selected to be newly infected
    # TODO: Confirm simulation day threshold to ensure infection does not happen
    # before 60 days into simulation
    if (length(newly_infected_farms > 0) & sim_day > 60) {
      # TODO: Complete call to he_infect_netpens()
      newly_infected_cages <- he_infect_netpens()
    }

  }
}
