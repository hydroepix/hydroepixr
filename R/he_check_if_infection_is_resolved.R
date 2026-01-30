#' Check if infection is resolved
#'
#' @param infected_net_pen_info data frame containing infected net pen info
#'
#' @return boolean value indicating whether the infection has run its course,
#'    i.e. all fish have progressed to the immune stage (includes dead)
#'
he_check_if_infection_is_resolved <- function(infected_net_pen_info) {
  return(all(
    (infected_net_pen_info$n_recovered + infected_net_pen_info$n_dead) ==
      infected_net_pen_info$n_total
  ))
}
