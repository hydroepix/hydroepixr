#' Read infected net pen output files
#'
#' @param infected_net_pen_output_file_name name of the infected net pen output files, should be the same as the one defined by the user in the he_define_output_params function
#' @param output_dir directory containing the specified infected net pen output files, likely the same as the one defined by the user in the he_define_output_params function
#'
#' @returns data.frame of up to 5 infected net pen output simulation files stacked together
#'
#' @importFrom readr read_csv
#'
#' @export
he_read_infected_net_pen_output_data <- function(
  infected_net_pen_output_file_name,
  output_dir
) {
  output_file_list <- list.files(output_dir)
  output_file_list <- output_file_list[grepl(
    infected_net_pen_output_file_name,
    output_file_list
  )]
  sim_dat <- data.frame()
  for (simulation_file_num in 1:length(output_file_list)) {
    new_sim_dat <- readr::read_csv(
      paste0(
        output_dir,
        "/",
        output_file_list[simulation_file_num]
      ),
      show_col_types = FALSE
    ) |>
      dplyr::mutate(simulation_num = simulation_file_num)
    sim_dat <- rbind(sim_dat, new_sim_dat)
  }
  sim_dat
}
