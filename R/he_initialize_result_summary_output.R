he_initialize_result_summary_output <- function() {
  environment$result_summary_output <-
    matrix(numeric(0), ncol = 10)
  environment$result_summary_file_name <- paste(run_id, "isa.txt")
  write.table(environment$result_summary_output,
              environment$result_summary_file_name,
              sep = " ")
}
