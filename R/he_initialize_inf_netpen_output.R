#' Initialize infected netpen output
#'
#' @param environment simulation environment
#' @param filepath path in which to store the infected netpen output file
#' @param inf_netpen_output_file_name name to use for creating and writing the
#'    infected netpen output file
#'
#' @return NA
#' @export
#'
he_initialize_inf_netpen_output <-
  function(environment,
           filepath,
           inf_netpen_output_file_name = "infected_netpens.txt") {

  # Set up infected netpen matrix and output file
  environment$inf_netpen_matrix_output <- matrix(numeric(0), ncol = 10)
  if (!is.null(environment$run_id)) {
    environment$inf_netpen_output_file_name <-
      paste(environment$run_id, inf_netpen_output_file_name, sep = "-")
  } else {
    environment$inf_netpen_output_file_name <- inf_netpen_output_file_name
  }
  he_write_inf_netpen_output(environment$inf_netpen_matrix,
                             environment$inf_netpen_output_file_name,
                             output_dir = filepath)
}
