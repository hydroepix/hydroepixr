#' Initialize infected netpen output
#'
#' @param environment simulation environment
#' @param filepath path in which to store the infected netpen output file
#'
#' @return NA
#' @export
#'
he_initialize_infected_netpen_output <- function(environment, filename) {
  # Set up infected netpen matrix and output file
  environment$infected_netpens <- matrix(numeric(0), ncol = 10)
  if (!is.null(environment$run_id)) {
    environment$infected_output_file_name <-
      paste(environment$run_id, "infected_netpens.txt", sep = "-")
  } else {
    environment$infected_output_file_name <- "infected_netpens.txt"
  }
  write.table(environment$infected_netpens, environment$infected_output_file_name, sep = " ")
}
