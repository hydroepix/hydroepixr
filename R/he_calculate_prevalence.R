#' Calculate the prevalence of infection
#'
#' @param n_infectious number of animals which are infectious
#' @param n_not_immune number of animals which cannot be infected
#'
#' @return prevalence of infected animals within a net pen or a farm
#'
he_calculate_prevalence <- function(n_infectious, n_not_immune) {
  prevalence <- n_infectious / n_not_immune
  prevalence[is.nan(prevalence)] <- 0
  return(prevalence)
}
