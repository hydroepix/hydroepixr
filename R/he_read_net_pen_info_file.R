#' Read in a net pen information file
#'
#' @param filepath a filepath to read a csv file from
#' @param verbose a logical value indicating whether to provide additional
#'    output messages on function progress
#'
#' @return data.frame of the csv file
#'
#' @export
#' @importFrom utils read.table

he_read_net_pen_info_file <- function(filepath, verbose = FALSE) {
  net_pen_info <- utils::read.table(
    filepath,
    sep = ",",
    dec = ".",
    header = TRUE
  )
  n_net_pens <- length(net_pen_info$species)
  # Check for expected columns and names
  net_pen_info_cols <- names(net_pen_info)
  expected_cols <- list(
    "net_pen_id",
    "farm_id",
    "net_pen_size",
    "baseline_mort",
    "species_id",
    "bay_management_id"
  )
  optional_cols <- list("initial_infection_status", "initial_time_infected")
  mismatched_cols <-
    setdiff(
      union(net_pen_info_cols, expected_cols),
      intersect(net_pen_info_cols, expected_cols)
    )

  if (length(mismatched_cols) == 0) {
    if (verbose) {
      message(
        paste0(
          "Optional infection status and infection time columns ",
          "included in net pen info file.",
          "Retrieved values from file."
        )
      )
    }
  } else if (identical(mismatched_cols, optional_cols)) {
    if (verbose) {
      message(
        paste0(
          "Note: Optional infection status and infection time columns ",
          "not included in net pen info file.",
          "Default values assigned."
        )
      )
    }
    net_pen_info$time_infected <- rep(Inf, n_net_pens)
  } else {
    stop(
      "Error: Unexpected column headers. Expected headers are: ",
      paste(expected_cols, collapse = ", "),
      "Optional headers are: ",
      # TODO: Is there a valid case where one optional column will be provided
      # but not the other??
      paste(optional_cols, collapse = ", "),
      "Headers in the provided file that do not match are: ",
      paste(mismatched_cols, collapse = ", ")
    )
  }

  # Check for non-unique net pen IDs
  if (
    length(unique(net_pen_info$net_pen_id)) < length(net_pen_info$net_pen_id)
  ) {
    stop(
      "Error: simulation failed. Net pen ID must be unique, even across farms. Duplicate Value: ",
      paste(
        unique(net_pen_info$net_pen_id[duplicated(net_pen_info$net_pen_id)]),
        collapse = ", "
      )
    )
  }
  return(net_pen_info)
}
