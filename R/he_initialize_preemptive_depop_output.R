#' Initialize preemptive depopulation output
#'
#' @param environment simulation environment
#' @param filepath path in which to store the preemption depopulation output file
#' @param file_name name to use for creating and writing the preemptive depopulation
#'    output file
#'
#' @return NA
#' @export
#'
he_initialize_preemptive_depop_output <- function(environment, filepath, file_name) {
  environment$preemptive_depop_output <-
    matrix(numeric(0), ncol = 3)
  if (!is.null(environment$run_id)) {
    environment$preemptive_depop_output_file_name <-
      paste(environment$run_id, "preemptive_depop.txt", sep = "-")
  } else {
    environment$preemption_output_file_name <- "preemptive_depop.txt"
  }
  write.table(
    environment$preemptive_depop_output,
    environment$preemptive_depop_output_file_name,
    col.names = FALSE,
    row.names = FALSE
  )
}
