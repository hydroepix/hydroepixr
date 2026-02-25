#' Polynomial distribution sampling function
#'
#' @param sample_n number of samples to take
#' @param probability numeric probability or vector of probabilities
#'
#' @return number of samples distributed into each bin by probability
#'
he_rpoly2 <- function(sample_n, probability) {
  return(tabulate(
    bin = sample(
      1:length(probability),
      size = sample_n,
      replace = TRUE,
      prob = probability
    ),
    nbins = length(probability)
  ))
}
