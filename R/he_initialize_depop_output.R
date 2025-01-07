#' Initialize depopulation output
#'
#' @param environment simulation environment
#' @param filepath path in which to store the depopulation output file
#' @param depop_file_name name to use for creating and writing the depopulation
#'    output file
#'
#' @return NA
#' @export
#'
he_initialize_depop_output <-
  function(environment,
           filepath,
           depop_file_name = "depop.txt") {
    # Assign depopulation values in environment
    environment$depop_matrix_output <-
      matrix(numeric(0), ncol = 3)
    if (!is.null(environment$run_id)) {
      environment$depop_output_file_name <-
        paste(environment$run_id, depop_file_name, sep = "-")
    } else {
      environment$depop_output_file_name <- depop_file_name
    }
    # Write file for storing output
    he_write_depop_output(
      environment$depop_matrix_output,
      environment$depop_output_file_name,
      output_dir = filepath
    )
  }
