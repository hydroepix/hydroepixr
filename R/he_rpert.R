##  with min a, mode l, and max
## b.
#' Generate n PERT beta random variates
#'
#' @param n number of PERT beta random variates to generate
#' @param a minimum value
#' @param l mode value
#' @param b maximum value
#'
#' @return vector of PERT beta random variates
#' @importFrom stats rbeta
#' @export
#'
#' @examples
#' rpert(10, 0.14, 0.4, 0.8)
he_rpert <- function(n, a, l, b) {
  mu <- (a + 4 * l + b) / 6

  if (mu == l) {
    v <- w <- 3

  }  else {

    v <- (mu - a) * (2 * l - a - b) / (l - mu) / (b - a)
    w <- v * (b - mu) / (mu - a)
  }

  a + (b - a) * stats::rbeta(n, v, w)
}
