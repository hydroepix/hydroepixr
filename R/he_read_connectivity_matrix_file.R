#' Read in a connectivity matrix file
#'
#' @param filepath a filepath to read a csv file from
#' @param connectivity_matrix_type type of connectivity matrix provided, either
#'    distance matrix or hydroconnectivity matrix
#'
#' @return data.frame of the csv file
#' @export
#'
he_read_connectivity_matrix_file <- function(filepath,
                                             connectivity_matrix_type = "distance") {
  connectivity_matrix <- utils::read.table(
    filepath,
    sep = ",",
    dec = ".",
    header = FALSE
  )
  if (connectivity_matrix_type == "distance") {
    # TODO: Add check to see if the distance matrix is symmetrical
    # TODO: check that number of rows is the same as the number of columns
    # TODO: check that the values in the matrix are symmetrical
  } else if (connectivity_matrix_type == "hydroconnectivity") {
    # TODO: Add checks for validity of hydroconnectivity matrix, if there
    # are any
  } else {
    stop("Invalid connectivity matrix type provided.
         Type must be either 'distance' or 'hydroconnectivity.'")
  }

  connectivity_matrix
}
