#' Write infected netpen output to file
#'
#' @param simulation_day day of the simulation from which the output is being
#'    generated
#' @param inf_farm_info data frame containing infected netpen data, initialized
#'    and updated over the course of a simulation run
#' @param inf_netpen_output_file_name string to name the infected netpen output
#'    file
#' @param output_dir string to indicate the directory in which the output file
#'    should be written
#'
#' @return NA
#' @export
#' @importFrom utils write.table
#'
he_write_inf_netpen_output <- function(simulation_day,
                                       inf_farm_info,
                                       inf_netpen_output_file_name,
                                       output_dir) {
  write.table(
    cbind(data.frame(simulation_day), inf_farm_info),
    file.path(output_dir,
              inf_netpen_output_file_name),
    append = TRUE,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE
  )
}
