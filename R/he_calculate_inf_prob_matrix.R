he_calculate_inf_prob_matrix <- function(dist_mat,
                                         farm_ids,
                                         farm_to_farm,
                                         vaccine_efficacy,
                                         farm_active,
                                         farm_susceptibility,
                                         farm_infectiousness,
                                         dist_mat_type = "seaway distance") {
  # Define probability matrix based on seaway distance or particle contact hours
  if (dist_mat_type == "seaway distance") {
    # seaway distance involves a calculation
    prob_matrix <- sapply(farm_ids,
                          FUN = he_generate_seaway_dist_prob,
                          dist_mat,
                          farm_to_farm,
                          vaccine_efficacy)
  } else if (connectivity_matrix_mode == "particle contact hours") {
    # particle contact hours means a hydroconnectivity matrix as input
    # matrix just needs to be subset with farm_ids
    prob_matrix <- dist_mat[farm_ids,]
  } else {
    stop("Invalid probability matrix type for distance-based infection
           calculation. Valid types are 'seaway distance' and 'particle contact
           hours'")
  }
  # TODO: this seems to assume that the probability matrix is always symmetrical
  # Do we need checks for this? Where?
  # The input for the indices seems to every single farm id?
  # See HEprocess_pens_toggle.R, line 234
  # Check how the probability matrix is used, may give clues
  # Reset diagonal to 0 for symmetrical matrix
  diag(prob_matrix) <- 0.0
  # Apply farm activity, infection susceptibility and farm infectiousness
  # to probability matrix, column by column
  prob_matrix <- apply(prob_matrix, 2, function(prob_col) {
    prob_col * farm_active * farm_susceptibility * farm_infectiousness
  })
  prob_matrix
}
