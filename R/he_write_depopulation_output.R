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
  if (!exists("depop_matrix")) {
    stop("No depopulation matrix initialized. Run
          `he_initialize_depopulation_output()` to initialize before attempting
          to write output.")
  }

  if (!exists("depop_file_name")) {
    stop("No depopulation output file name initialized.
          Run `he_initialize_depopulation_output()` to assign
          a custom or default name to the output file.")
  }

  write.table(
    depop_matrix,
    file.path(output_dir,
              depop_file_name),
    append = append,
    col.names = FALSE,
    row.names = FALSE
  )
}
