#' Calculate the prevalence of disease
#'
#' @param n_infectious number of animals which are infectious
#' @param disease_stage_distribution number of animals which are not immune to infection
#'
#' @export
#'
he_calculate_prevalence <- function(n_infectious, n_not_immune) {
  prevalence <- n_infectious / n_not_immune
  prevalence[is.nan(prevalence)] <- 0
  prevalence
}
