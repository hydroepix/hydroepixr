he_initialize_infected_cage_output <- function(environment, filename) {
  # Set up infected cage matrix and output file
  environment$infected_cages <- matrix(numeric(0), ncol = 10)
  if (!is.null(environment$run_id)) {
    environment$infected_output_file_name <-
      paste(environment$run_id, "infected_cages.txt", sep = "-")
  } else {
    environment$infected_output_file_name <- "infected_cages.txt"
  }
  write.table(environment$infected_cages, environment$infected_output_file_name, sep = " ")
}
