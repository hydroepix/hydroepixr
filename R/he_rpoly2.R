#' RPoly2 ?
#'
#' @param np ?
#'
#' @return ?
#' @export
#'
he_rpoly2 <- function(np) {
  tabulate(bin =
             sample(
               1:(length(np) - 1),
               size = np[1],
               replace = TRUE,
               prob = np[-1]
             ),
           nbins = length(np) - 1)
}
