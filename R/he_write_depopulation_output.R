#' Write depopulation output to file
#'
#' @param environment simulation environment
#' @param append logical indicating whether to overwrite an existing file or
#'    append data
#'
#' @return NA
#' @export
#'
he_write_depopulation_output <- function(environment, output_dir = "", append = FALSE) {
  if (is.null(environment$depopulation_matrix_output)) {
    stop("No depopulation matrix initialized. Run
          `he_initialize_depopulation_output()` to initialize before attempting
          to write output.")
  }

  if (is.null(environment$depopulation_output_file_name)) {
    stop("No depopulation output file name initialized.
          Run `he_initialize_depopulation_output()` to assign
          a custom or default name to the output file.")
  }

  message(paste0("Writing to ",
                 file.path(output_dir,
                           environment$depopulation_output_file_name)))
  write.table(
    environment$depopulation_matrix_output,
    file.path(output_dir, environment$depopulation_output_file_name),
    append = append,
    col.names = FALSE,
    row.names = FALSE
  )
}
