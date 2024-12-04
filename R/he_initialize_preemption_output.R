he_initialize_preemption_output <- function() {
  environment$preempted_matrix_output <-
    matrix(numeric(0), ncol = 3)
  preempted_output_file_name <-
    paste(run_id, "preempted_farms.txt", sep = "-")

  write.table(
    preempted_matrix_output,
    preempted_output_file_name,
    col.names = FALSE,
    row.names = FALSE
  )
}
