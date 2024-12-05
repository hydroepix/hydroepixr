he_initialize_depopulation_output <- function(environment, filepath) {
  environment$depopulation_matrix_output <-
    matrix(numeric(0), ncol = 3)
  if (!is.null(environment$run_id)) {
    environment$depopulation_output_file_name <-
      paste(environment$run_id, "depopulated_farms.txt", sep = "-")
  } else {
    environment$depopulation_output_file_name <- "depopulated_farms.txt"
  }
  write.table(
    environment$depopulation_matrix_output,
    paste0(filepath, environment$depopulation_output_file_name),
    col.names = FALSE,
    row.names = FALSE
  )
}
