he_identify_netpens_for_infection <- function(farm_info, inf_farm_id) {
  # Netpens must be in a newly infected farm, have infection status of 1,
  # and be active
  farm_info$netpen_id[farm_info$farm_id == inf_farm_id &
                        farm_info$status == 1 &
                        farm_info$farm_active]
}
