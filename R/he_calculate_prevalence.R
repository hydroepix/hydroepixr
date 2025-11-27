he_calculate_prevalence <- function(n_infectious, n_not_immune) {
  prevalence <- n_infectious / n_not_immune
  prevalence[is.nan(prevalence)] <- 0
  prevalence
}
