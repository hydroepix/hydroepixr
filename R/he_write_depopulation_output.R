#' Write depopulation output to file
#'
#' @param depop_matrix matrix containing depopulation variables, initialized
#'    and updated over the course of a simulation run
#' @param depop_file_name string to name the depopulation output file
#' @param output_dir string to indicate the directory in which the output file
#'    should be written
#' @param append logical indicating whether to overwrite an existing file or
#'    append data
#'
#' @return NA
#' @export
#'
he_write_depopulation_output <- function(depop_matrix,
                                         depop_file_name = "depopulated_farms.txt",
                                         output_dir = "",
                                         append = FALSE) {
  write.table(
    depop_matrix,
    file.path(output_dir,
              depop_file_name),
    append = append,
    col.names = FALSE,
    row.names = FALSE
  )
}
