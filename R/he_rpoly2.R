#' RPoly2 ?
#'
#' @param sample_num number of samples to take
#' @param probability numeric probability or vector of probabilities
#'
#' @return number of samples distributed into each bin by probability
#' @export
#'
he_rpoly2 <- function(sample_num, probability) {
  tabulate(bin =
             sample(
               1:length(probability),
               size = sample_num,
               replace = TRUE,
               prob = probability
             ),
           nbins = length(probability))
}
