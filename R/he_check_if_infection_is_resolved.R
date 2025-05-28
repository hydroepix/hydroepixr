#' Check if infection is resolved
#'
#' @param infected_netpen_info data frame containing infected netpen info
#'
#' @returns boolean value indicating whether the infection has run its course,
#'    i.e. all fish have progressed to the immune stage (includes dead)
#' @export
#'
he_check_if_infection_is_resolved <- function(infected_netpen_info) {
  all((infected_netpen_info$n_recovered + infected_netpen_info$n_dead) == infected_netpen_info$n_total)
}
