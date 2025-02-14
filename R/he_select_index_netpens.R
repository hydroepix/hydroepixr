#' Select Index Netpens
#'
#' @param farm_info data frame of farm information
#' @param farm_id identifier for farm to infect
#' @param num_inf_netpens
#'
#' @return netpen identifier(s) of initially infected netpens
#' @export
#'
he_select_index_netpens <-
  function(farm_info, netpen_ids = NULL, farm_id = NULL, num_inf_netpens = 1) {
    # If no netpen id is provided, select randomly from the netpens with the
    # provided farm id
    if(is.null(netpen_ids)) {
      if(is.null(farm_id)) {
        stop("Either a specific netpen ID or a farm ID must be provided in order
             to select an index netpen.")
      }
      farm_id_netpens <- farm_info[farm_info$farm_id == farm_id]
      if (length(farm_id_netpens) == 0) {
        stop("Farm ID provided not found in farm info. Please confirm the
             provided farm ID exists in the farm info file.")
      }
      # Randomly sample the specified number of netpens from the specified farm
      # as index netpens
      inf_netpen_ids <- sample(farm_id_netpens$netpen_ids,
                               size = num_inf_netpens)
    # Otherwise, simply check if the netpen ids provided are valid
    } else {
      inf_netpen_ids <- subset(farm_info, farm_info$netpen_id %in% netpen_ids)
      if (length(inf_netpen_ids) < length(netpen_ids)) {
        stop("Invalid netpen ID identified. Please confirm all provided netpen
             IDs exist in the farm info file.")
      }
    }
    inf_netpen_ids
  }
