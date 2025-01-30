#' Calculates probability of infection given a set of seaway distances
#'
#' @param farm_id farm identifiers to apply the calculation to
#' @param dist_mat matrix of seaway distances
#' @param farm_to_farm scaling parameter for between-farm infection transmission
#' @param vaccine_efficacy product of the manufacturer-reported vaccine efficacy
#'    and the population coverage of the vaccine
#'
#' @return scalar or vector of distance-scaled infection probabilities
#' @export
#'
he_generate_seaway_dist_prob <- function(farm_id,
                                         dist_mat,
                                         farm_to_farm,
                                         vaccine_efficacy) {
  (exp(-farm_to_farm * dist_mat[farm_id,] / 1000) * (1 - vaccine_efficacy)) / 30
}
