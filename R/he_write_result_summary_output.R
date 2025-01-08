#' Write pre-emptive depopulation output to file
#'
#' @param result_summary_matrix matrix containing depopulation variables, initialized
#'    and updated over the course of a simulation run
#' @param result_summary_file_name string to name the depopulation output file
#' @param output_dir string to indicate the directory in which the output file
#'    should be written
#' @param append logical indicating whether to overwrite an existing file or
#'    append data
#'
#' @return NA
#' @export
#'
he_write_result_summary_output <-
  function(result_summary_matrix,
           result_summary_file_name = "result_summary.txt",
           output_dir = "",
           append = FALSE) {
  write.table(
    result_summary_matrix,
    file.path(output_dir,
              result_summary_file_name),
    sep = " ",
    append = append,
    col.names = FALSE,
    row.names = FALSE
  )
}
