#' Calculate the prevalence of disease
#'
#' @param n_infectious number of animals which are infectious
#' @param n_not_immune number of animals which cannot be infected

#'
#' @export
#'
he_calculate_prevalence <- function(n_infectious, n_not_immune) {
  prevalence <- n_infectious / n_not_immune
  prevalence[is.nan(prevalence)] <- 0
  prevalence
}
