#' Write names of columns to output file
#'
#' @param output_column_names names of columns to write to file
#' @param output_dir directory of file to be written to
#' @param output_file_name name of file to be written to
#'
#' @return NA
#' @export
#'
he_write_output_columns <- function(output_column_names,
                                    output_dir,
                                    output_file_name) {
  write.table(
    t(output_column_names),
    file.path(output_dir,
              output_file_name),
    # ensures that the file will be overwritten on a subsequent simulation
    append = FALSE,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE
  )
}
