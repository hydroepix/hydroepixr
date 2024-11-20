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
  mismatched_columns <-
    setdiff(
      union(farm_info_columns, expected_columns),
      intersect(farm_info_columns, expected_columns)
    )
  if (length(mismatched_columns > 0)) {
    stop(
      "Unexpected column headers. Expected headers are: ",
      paste(expected_columns, collapse = ", "),
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
