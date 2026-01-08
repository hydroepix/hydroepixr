#' Simulate a day of infection transmission dynamics
#'
#' @param farm_id id of farm within which between-net-pen infection is to be simulated
#' @param net_pen_info data frame of information on net pens
#' @param infected_net_pen_info data frame of information on infected net pens
#'
#' @return number of net pens which are susceptible to infection in the specified farm
#' @export
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#'
he_calculate_susceptible_net_pens <- function(
  farm_id,
  net_pen_info,
  infected_net_pen_info
) {
  # Currently calculated as all net pen IDs minus those in the infected net pen data frame
  # Is there a more efficient way to do this?
  # Option: include status in net pen info (however, this would require modifying another data frame
  # throughout the simulation, which would complicate things)
  all_net_pen_ids <- net_pen_info |>
    dplyr::select(c(farm_id, net_pen_id)) |>
    dplyr::filter(farm_id == !!farm_id) |>
    dplyr::pull(net_pen_id)
  infected_net_pen_ids <- infected_net_pen_info |>
    dplyr::select(c(farm_id, net_pen_id)) |>
    dplyr::filter(farm_id == !!farm_id) |>
    dplyr::pull(net_pen_id)
  susceptible_net_pens <- setdiff(all_net_pen_ids, infected_net_pen_ids)
  n_susceptible_net_pens <- length(susceptible_net_pens)
  n_susceptible_net_pens
}
