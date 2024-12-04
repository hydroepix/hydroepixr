he_initialize_depopulation_output <- function() {
  environment$depopulated_matrix_output <-
    matrix(numeric(0), ncol = 3)
  depopulated_output_file_name <-
    paste(run_id, "depopulated_farms.txt", sep = "-")
  write.table(
    depopulated_matrix_output,
    depopulated_output_file_name,
    col.names = FALSE,
    row.names = FALSE
  )
}
