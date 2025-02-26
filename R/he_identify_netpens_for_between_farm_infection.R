#' Generates the subset of netpens which can be infected from a selection
#'
#' @param farm_info matrix of farm information
#' @param inf_farm_id identifier of the farm which is to be infected
#'
#' @return vector of netpen identifiers for the given farm, where those netpens
#'    can be validly infected
#' @export
#'
he_identify_netpens_for_between_farm_infection <- function(farm_info, inf_farm_id) {
  # Netpens must be in a newly infected farm, have infection status of 1,
  # and be active
  # TODO: Is farm_active actually netpen_active?
  farm_info$netpen_id[farm_info$farm_id == inf_farm_id &
                        farm_info$status == 1 &
                        farm_info$farm_active]
}
