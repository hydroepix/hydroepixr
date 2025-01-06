#' Initialize depopulation output
#'
#' @param environment simulation environment
#' @param filepath path in which to store the depopulation output file
#' @param file_name name to use for creating and writing the depopulation
#'    output file
#'
#' @return NA
#' @export
#'
he_initialize_depopulation_output <-
  function(environment, filepath, file_name = "depopulated_farms.txt") {
    environment$depopulation_matrix_output <-
      matrix(numeric(0), ncol = 3)
    if (!is.null(environment$run_id)) {
      environment$depopulation_output_file_name <-
        paste(environment$run_id, file_name, sep = "-")
    } else {
      environment$depopulation_output_file_name <- file_name
    }
    he_write_depopulation_output(environment$depopulation_matrix_output,
                                 depop_file_name = environment$depopulation_output_file_name,
                                 output_dir = filepath)
  }
