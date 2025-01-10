#' Write survey output to file
#'
#' @param surveyed_output_matrix matrix containing surveyed farm variables, initialized
#'    and updated over the course of a simulation run
#' @param survey_output_file_name string to name the depopulation output file
#' @param output_dir string to indicate the directory in which the output file
#'    should be written
#' @param append logical indicating whether to overwrite an existing file or
#'    append data
#'
#' @return NA
#' @export
#'
he_write_survey_output <- function(surveyed_output_matrix,
                                   survey_output_file_name =
                                     "surveyed_farms.txt",
                                   output_dir = "",
                                   append = FALSE) {

  write.table(
    surveyed_output_matrix,
    file.path(output_dir, survey_output_file_name),
    append = append,
    col.names = FALSE,
    row.names = FALSE
  )
}

