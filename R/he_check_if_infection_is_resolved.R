#' Check if infection is resolved
#'
#' @param infected_net_pen_info data frame containing infected net pen info
#'
#' @returns boolean value indicating whether the infection has run its course,
#'    i.e. all fish have progressed to the immune stage (includes dead)
#' @export
#'
he_check_if_infection_is_resolved <- function(infected_net_pen_info) {
  all(
    (infected_net_pen_info$n_recovered + infected_net_pen_info$n_dead) ==
      infected_net_pen_info$n_total
  )
}
