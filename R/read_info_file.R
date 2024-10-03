#' Read in a farm information file
#'
#' @param filepath a filepath to read a csv file from
#'
#' @return data.frame of the csv file
#' @export
#'
#' @examples
read_farm_info_file <- function(filepath) {
  farm_info <- read.table(
    filepath,
    sep = ",",
    dec = ".",
    header = TRUE
  )
  # TODO: check for expected columns and names
  # Check for non-unique IDs
  if (length(unique(farm_info$cage_id)) < length(farm_info$cage_id)) {
    stop("Cage ID numbers are not unique. Simulations Fails. Duplicate Value: ",
         paste(unique(farm_info$cage_id[duplicated(farm_info$cage_id)]),collapse=", "))
  }
  farm_info
}
