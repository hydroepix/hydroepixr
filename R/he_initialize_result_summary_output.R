#' Initialize result summary output
#'
#' @param environment simulation environment
#' @param filepath path in which to store the results summary output file
#'
#' @return NA
#' @export
#'
he_initialize_result_summary_output <-
  function(environment, filepath) {
    environment$result_summary <-
      matrix(numeric(0), ncol = 10)
    if (!is.null(environment$run_id)) {
      environment$result_summary_file_name <-
        paste(environment$run_id, "result_summary.txt", sep = "-")
    } else {
      environment$result_summary_file_name <- "result_summary.txt"
    }
    write.table(environment$result_summary,
                environment$result_summary_file_name,
                sep = " ")
  }
