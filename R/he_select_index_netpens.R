#' Select Index Net Pens
#'
#' @param net_pen_info data frame of net pen information
#' @param net_pen_ids identifiers of specific net pens to be infected
#' @param farm_id identifier for farm to be infected first, if no net pen
#'    identifiers are supplied
#' @param n_infected_net_pens number of randomly selected net pens to be infected in
#'    the specified farm, if no net pen identifiers are supplied
#'
#' @return net pen identifier(s) of initially infected net pens
#' @export
#'
he_select_index_net_pens <-
  function(
    net_pen_info,
    net_pen_ids = NULL,
    farm_id = NULL,
    n_infected_net_pens = 1
  ) {
    # TODO: Check for cases where both net_pen_ids and farm_id are provided?
    # If no net pen id is provided, select randomly from the net pens with the
    # provided farm id
    if (is.null(net_pen_ids)) {
      if (is.null(farm_id)) {
        stop(
          "Either a specific net pen ID or a farm ID must be provided in order
             to select an index net pen."
        )
      }
      farm_id_net_pens <- net_pen_info[net_pen_info$farm_id == farm_id, ]
      if (nrow(farm_id_net_pens) == 0) {
        stop(
          "Farm ID provided not found in net pen info. Please confirm the
             provided farm ID exists in the net pen info file."
        )
      }
      # Randomly sample the specified number of net pens from the specified farm
      # as index net pens
      infected_net_pen_ids <- sample(
        farm_id_net_pens$net_pen_id,
        size = n_infected_net_pens
      )
      # Otherwise, simply check if the net pen ids provided are valid
    } else {
      # Net pen id is not null, in which case farm id should be
      if (is.null(farm_id)) {
        infected_net_pen_ids <- subset(
          net_pen_info,
          net_pen_info$net_pen_id %in% net_pen_ids
        )$net_pen_id
        if (length(infected_net_pen_ids) < length(net_pen_ids)) {
          # TODO: Customize message to indicate which net pen ID is invalid?
          stop(
            "Invalid net pen ID identified. Please confirm all provided net pen
             IDs exist in the net pen info file."
          )
        }
      } else {
        stop("Either net pen ID or farm ID should be provided, not both.")
      }
    }
    infected_net_pen_ids
  }
