#' Select Index Netpens
#'
#' @param netpen_info data frame of netpen information
#' @param netpen_ids identifiers of specific netpens to be infected
#' @param farm_id identifier for farm to be infected first, if no netpen
#'    identifiers are supplied
#' @param n_infected_netpens number of randomly selected netpens to be infected in
#'    the specified farm, if no netpen identifiers are supplied
#'
#' @return netpen identifier(s) of initially infected netpens
#' @export
#'
he_select_index_netpens <-
  function(netpen_info, netpen_ids = NULL, farm_id = NULL, n_infected_netpens = 1) {
    # TODO: Check for cases where both netpen_ids and farm_id are provided?
    # If no netpen id is provided, select randomly from the netpens with the
    # provided farm id
    if(is.null(netpen_ids)) {
      if(is.null(farm_id)) {
        stop("Either a specific netpen ID or a farm ID must be provided in order
             to select an index netpen.")
      }
      farm_id_netpens <- netpen_info[netpen_info$farm_id == farm_id,]
      if (nrow(farm_id_netpens) == 0) {
        stop("Farm ID provided not found in netpen info. Please confirm the
             provided farm ID exists in the netpen info file.")
      }
      # Randomly sample the specified number of netpens from the specified farm
      # as index netpens
      infected_netpen_ids <- sample(farm_id_netpens$netpen_id,
                               size = n_infected_netpens)
    # Otherwise, simply check if the netpen ids provided are valid
    } else {
      # Netpen id is not null, in which case farm id should be
      if (is.null(farm_id)) {
        infected_netpen_ids <- subset(netpen_info, netpen_info$netpen_id %in% netpen_ids)$netpen_id
        if (length(infected_netpen_ids) < length(netpen_ids)) {
          # TODO: Customize message to indicate which netpen ID is invalid?
          stop("Invalid netpen ID identified. Please confirm all provided netpen
             IDs exist in the netpen info file.")
        }
      } else {
        stop("Either netpen ID or farm ID should be provided, not both.")
      }

    }
    infected_netpen_ids
  }
