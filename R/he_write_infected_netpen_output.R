#' Write infected netpen output to file
#'
#' @param infected_netpen_info data frame containing infected netpen data, initialized
#'    and updated over the course of a simulation run
#' @param infected_netpen_output_file_name string to name the infected netpen output
#'    file
#' @param output_dir string to indicate the directory in which the output file
#'    should be written
#'
#' @return NA
#' @export
#' @importFrom utils write.table
#'
he_write_infected_netpen_output <- function(infected_netpen_info,
                                       infected_netpen_output_file_name,
                                       output_dir) {
  write.table(
    infected_netpen_info,
    file.path(output_dir,
              infected_netpen_output_file_name),
    append = TRUE,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE
  )
}
