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
    sep = ";",
    dec = ".",
    header = TRUE
  )
}
