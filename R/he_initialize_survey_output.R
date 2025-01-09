#' Initialize survey output
#'
#' @param environment simulation environment
#' @param filepath path in which to store the survey output file
#' @param survey_output_file_name name to use for creating and writing the
#'    surveyed farm output file
#'
#' @return NA
#' @export
#'
he_initialize_survey_output <-
  function(environment,
           filepath,
           survey_output_file_name = "surveyed_farms.txt") {
  environment$surveyed_matrix_output <- matrix(numeric(0), ncol = 3)
  if (!is.null(environment$run_id)) {
    environment$survey_output_file_name <-
      paste(environment$run_id, survey_output_file_name, sep = "-")
  } else {
    environment$survey_output_file_name <- survey_output_file_name
  }
  he_write_survey_output(
    environment$surveyed_matrix_output,
    environment$survey_output_file_name,
    output_dir = filepath
  )
}
