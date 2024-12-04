he_initialize_survey_output <- function() {
  environment$surveyed_matrix_output <- matrix(numeric(0), ncol = 3)
  surveyed_output_file_name <-
    paste(run_id, "surveyed_farms.txt", sep = "-")
  write.table(
    surveyed_matrix_output,
    surveyed_output_file_name,
    col.names = FALSE,
    row.names = FALSE
  )
}
