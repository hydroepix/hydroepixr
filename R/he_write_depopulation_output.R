#' Write depopulation output to file
#'
#' @param environment simulation environment
#' @param append logical indicating whether to overwrite an existing file or
#'    append data
#'
#' @return NA
#' @export
#'
he_write_depopulation_output <- function(environment, append = FALSE) {
  write.table(
    environment$depopulation_matrix_output,
    paste0(environment$filepath, environment$depopulation_output_file_name),
    append = append,
    col.names = FALSE,
    row.names = FALSE
  )
}
