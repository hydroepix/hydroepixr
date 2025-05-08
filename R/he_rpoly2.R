#' RPoly2 ?
#'
#' @param sample_n number of samples to take
#' @param probability numeric probability or vector of probabilities
#'
#' @return number of samples distributed into each bin by probability
#' @export
#'
he_rpoly2 <- function(sample_n, probability) {
  tabulate(bin =
             sample(
               1:length(probability),
               size = sample_n,
               replace = TRUE,
               prob = probability
             ),
           nbins = length(probability))
}
