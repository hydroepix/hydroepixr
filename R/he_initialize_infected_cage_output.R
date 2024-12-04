he_initialize_infected_cage_output <- function(run_id, filename) {
  # Set up infected cage matrix and output file
  environment$all_inf_cages <- matrix(numeric(0), ncol = 10)
  environment$infected_output_file_name <-
    paste(run_id, "all_inf_cages.txt", sep = "-")
  write.table(all_inf_cages, infected_output_file_name, sep = " ")
}
