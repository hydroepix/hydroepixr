#' Write infected net pen output to file
#'
#' @param infected_net_pen_info data frame containing infected net pen data, initialized
#'    and updated over the course of a simulation run
#' @param infected_net_pen_output_file_name string to name the infected net pen output
#'    file
#' @param output_dir string to indicate the directory in which the output file
#'    should be written
#'
#' @return NA
#'
#' @importFrom utils write.table
#'
he_write_infected_net_pen_output <- function(
  infected_net_pen_info,
  infected_net_pen_output_file_name,
  output_dir
) {
  write.table(
    infected_net_pen_info,
    file.path(output_dir, infected_net_pen_output_file_name),
    append = TRUE,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE
  )
}
