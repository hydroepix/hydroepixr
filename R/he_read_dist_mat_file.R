#' Read in a distance matrix file
#'
#' @param filepath a filepath to read a csv file from
#'
#' @return data.frame of the csv file
#' @export
#'
he_read_dist_mat_file <- function(filepath) {
  dist_mat <- utils::read.table(
    filepath,
    sep = ",",
    dec = ".",
    header = FALSE
  )
  # TODO: Add check to see if the distance matrix is symmetrical
    # TODO: check that number of rows is the same as the number of columns
    # TODO: check that the values in the matrix are symmetrical
}
