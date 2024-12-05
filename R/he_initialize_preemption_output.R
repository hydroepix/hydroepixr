he_initialize_preemption_output <- function(environment, filepath) {
  environment$preemption_output <-
    matrix(numeric(0), ncol = 3)
  if (!is.null(environment$run_id)) {
    environment$preemption_output_file_name <-
      paste(environment$run_id, "preempted.txt", sep = "-")
  } else {
    environment$preemption_output_file_name <- "preempted.txt"
  }
  write.table(
    environment$preemption_output,
    environment$preemption_output_file_name,
    col.names = FALSE,
    row.names = FALSE
  )
}
