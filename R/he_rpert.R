#' Generate n PERT beta random variates
#'
#' @param n number of PERT beta random variates to generate
#' @param min minimum value
#' @param mode mode value
#' @param max maximum value
#'
#' @return vector of PERT beta random variates
#'
#' @importFrom stats rbeta
#'
he_rpert <- function(n, min, mode, max) {
  expected <- (min + 4 * mode + max) / 6

  # isTRUE(all.equal(...)) is used to accommodate minor floating point
  # representation and arithmetic differences
  if (isTRUE(all.equal(expected, mode))) {
    v <- w <- 3
  } else {
    v <- (expected - min) *
      (2 * mode - min - max) /
      (mode - expected) /
      (max - min)
    w <- v * (max - expected) / (expected - min)
  }
  return(min + (max - min) * stats::rbeta(n, v, w))
}
