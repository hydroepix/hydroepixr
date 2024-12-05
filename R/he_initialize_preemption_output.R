he_initialize_preemption_output <- function(environment, filepath) {
  environment$preemption_output <-
    matrix(numeric(0), ncol = 3)
  preemption_output_file_name <-
    paste(environment$run_id, "preempted.txt", sep = "-")

  write.table(
    environment$preemption_output,
    environment$preemption_output_file_name,
    col.names = FALSE,
    row.names = FALSE
  )
}
