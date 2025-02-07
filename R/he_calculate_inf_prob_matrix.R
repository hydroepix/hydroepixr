#' Calculates a symmetrical matrix of farm infection probabilities
#'
#' @param connectivity_matrix connectivity matrix in the form of a seaway
#'    distance matrix (in kilometers)or a hydroconnectivity matrix
#' @param farm_ids farm identifiers for which to calculate infection probabilities
#' @param farm_to_farm scaling parameter for between-farm infection transmission
#' @param vaccine_efficacy product of the manufacturer-reported vaccine efficacy
#'    and the population coverage of the vaccine
#' @param farm_active logical indicating whether or not the farm is active
#' @param farm_susceptibility logical indicating whether or not the farm is susceptible
#' @param farm_infectiousness logical indicating whether or not the farm is infectious
#' @param connectivity_matrix_type type of connectivity matrix provided, either
#'    distance matrix or hydroconnectivity matrix
#'
#' @return a symmetrical(?) matrix of farm infection probabilities
#' @export
#'
he_calculate_inf_prob_matrix <- function(connectivity_matrix,
                                         farm_ids,
                                         farm_to_farm,
                                         vaccine_efficacy,
                                         farm_active,
                                         farm_susceptibility,
                                         farm_infectiousness,
                                         connectivity_matrix_type = "distance") {
  # Define probability matrix based on seaway distance or particle contact hours
  if (connectivity_matrix_type == "distance") {
    # seaway distance matrix requires a scaling calculation
    prob_matrix <- sapply(farm_ids,
                          FUN = he_generate_seaway_dist_prob,
                          connectivity_matrix,
                          farm_to_farm,
                          vaccine_efficacy)
  } else if (connectivity_matrix_mode == "hydroconnectivity") {
    # hydrconnectivity matrix just needs to be subset with farm_ids
    prob_matrix <- connectivity_matrix[farm_ids,]
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
