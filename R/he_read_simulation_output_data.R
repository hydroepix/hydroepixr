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
