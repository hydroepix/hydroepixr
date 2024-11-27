#' Read in a farm information file
#'
#' @param filepath a filepath to read a csv file from
#'
#' @return data.frame of the csv file
#' @export
#' @importFrom utils read.table

he_read_farm_info_file <- function(filepath) {
  farm_info <- utils::read.table(
    filepath,
    sep = ",",
    dec = ".",
    header = TRUE
  )
  # Check for expected columns and names
  # TODO: Confirm whether all of these columns are necessary for the model
  # or is a specific subset sufficient? The check should change accordingly.
  farm_info_columns <- names(farm_info)
  expected_columns <- list(
    "cage_id",
    "farm_id",
    "cage_size",
    "baseline_mort",
    "farm_type",
    "bmaid"
  )
  optional_columns <- list(
    "initial_infection_status",
    "initial_time_infected"
  )
  mismatched_columns <-
    setdiff(
      union(farm_info_columns, expected_columns),
      intersect(farm_info_columns, expected_columns)
    )

  if (length(mismatched_columns) == 0) {
    if (environment$verbose) {
      message("Optional infection status and infection time columns included in farm info. Retrieved values from file.")
    }
  } else if (identical(mismatched_columns, optional_columns)) {
    if (environment$verbose) {
      message("Optional infection status and infection time columns not included in farm info. Default values assigned.")
    }
    environment$farm_info$status <- rep(1, length(environment$farm_info$farm_type))
    environment$farm_info$time_infected <- rep(Inf, g_max_farms)
  } else {
    stop(
      "Unexpected column headers. Expected headers are: ",
      paste(expected_columns, collapse = ", "),
      "Optional headers are: ",
      # TODO: Is there a valid case where one optional column will be provided but not the other??
      paste(optional_columns, collapse = ", "),
      "Headers in the provided file that do not match are: ",
      paste(mismatched_columns, collapse = ", ")
    )
  }

  # Check for non-unique IDs
  if (length(unique(farm_info$cage_id)) < length(farm_info$cage_id)) {
    stop("Cage ID numbers are not unique. Simulations Fails. Duplicate Value: ",
         paste(unique(farm_info$cage_id[duplicated(farm_info$cage_id)]), collapse = ", "))
  }

  farm_info
}
