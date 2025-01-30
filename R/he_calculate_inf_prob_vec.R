#' Converts an infection probability matrix into an infection probability vector
#'
#' @param prob_matrix symmetrical matrix of infection probabilities
#'
#' @return a vector of infection probabilities
#' @export
#'
he_calculate_inf_prob_vec <- function(prob_matrix) {
  prob_matrix <- 1 - prob_matrix
  # Calculate the product of each column in the probability matrix, generating
  # a vector which is the inverse of the infection probabilities for each farm
  inverse_inf_prob_vec <- apply(prob_matrix, 2, function(column) prod(column))
  1 - inverse_inf_prob_vec
}
