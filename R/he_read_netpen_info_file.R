#' Read in a netpen information file
#'
#' @param filepath a filepath to read a csv file from
#' @param verbose a logical value indicating whether to provide additional
#'    output messages on function progress
#'
#' @return data.frame of the csv file
#' @export
#' @importFrom utils read.table

he_read_netpen_info_file <- function(filepath, verbose = FALSE) {
  netpen_info <- utils::read.table(filepath,
                                 sep = ",",
                                 dec = ".",
                                 header = TRUE)
  n_netpens <- length(netpen_info$species)
  # Check for expected columns and names
  netpen_info_cols <- names(netpen_info)
  expected_cols <- list("netpen_id",
                           "farm_id",
                           "netpen_size",
                           "baseline_mort",
                           "species_id",
                           "bay_management_id")
  optional_cols <- list("initial_infection_status",
                           "initial_time_infected")
  mismatched_cols <-
    setdiff(
      union(netpen_info_cols, expected_cols),
      intersect(netpen_info_cols, expected_cols)
    )

  if (length(mismatched_cols) == 0) {
    if (verbose) {
      message(
        paste0("Optional infection status and infection time columns ",
        "included in netpen info file.",
        "Retrieved values from file.")
      )
    }
  } else if (identical(mismatched_cols, optional_cols)) {
    if (verbose) {
      message(
        paste0("Optional infection status and infection time columns ",
        "not included in netpen info file.",
        "Default values assigned.")
      )
    }
    netpen_info$infection_status <- rep(1, n_netpens)
    netpen_info$time_infected <- rep(Inf, n_netpens)
  } else {
    stop(
      "Unexpected column headers. Expected headers are: ",
      paste(expected_cols, collapse = ", "),
      "Optional headers are: ",
      # TODO: Is there a valid case where one optional column will be provided
      # but not the other??
      paste(optional_cols, collapse = ", "),
      "Headers in the provided file that do not match are: ",
      paste(mismatched_cols, collapse = ", ")
    )
  }

  # Check for non-unique IDs
  if (length(unique(netpen_info$netpen_id)) < length(netpen_info$netpen_id)) {
    stop(
      "Netpen ID numbers are not unique. Simulations Fails. Duplicate Value: ",
      paste(unique(netpen_info$netpen_id[duplicated(netpen_info$netpen_id)]), collapse = ", ")
    )
  }
  netpen_info
}
