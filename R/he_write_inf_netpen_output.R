#' Write infected netpen output to file
#'
#' @param inf_netpen_matrix matrix containing infected netpen data, initialized
#'    and updated over the course of a simulation run
#' @param inf_netpen_output_file_name string to name the infected netpen output
#'    file
#' @param output_dir string to indicate the directory in which the output file
#'    should be written
#' @param append logical indicating whether to overwrite an existing file or
#'    append data
#'
#' @return NA
#' @export
#'
he_write_inf_netpen_output <- function(inf_netpen_matrix,
                                       inf_netpen_output_file_name =
                                         "infected_netpens.txt",
                                       output_dir = "",
                                       append = FALSE) {
  write.table(
    inf_netpen_matrix,
    file.path(output_dir,
              inf_netpen_output_file_name),
    append = append,
    sep = " ",
    col.names = FALSE,
    row.names = FALSE
  )
}
