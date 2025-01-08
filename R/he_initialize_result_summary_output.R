#' Initialize result summary output
#'
#' @param environment simulation environment
#' @param filepath path in which to store the results summary output file
#' @param result_summary_file_name name to use for creating and writing the
#'    result summary output file
#'
#' @return NA
#' @export
#'
he_initialize_result_summary_output <-
  function(environment,
           filepath,
           result_summary_output_file_name = "result_summary.txt") {
    environment$result_summary_matrix_output <-
      matrix(numeric(0), ncol = 10)
    if (!is.null(environment$run_id)) {
      environment$result_summary_output_file_name <-
        paste(environment$run_id, result_summary_output_file_name, sep = "-")
    } else {
      environment$result_summary_output_file_name <-
        result_summary_output_file_name
    }
    he_write_result_summary_output(
      environment$result_summary_matrix_output,
      environment$result_summary_output_file_name,
      output_dir = filepath
    )
  }
