#' Define model-level variables to customize model output
#'
#' @param model_env environment in which environment variables are stored and
#'   managed
#' @param output_dir directory into which output should be written
#' @param model_run_id identifier for this run of the model
#' @param infected_net_pen_output_file_name base file name to which the run ID and
#'    simulation number will be appended to uniquely identify a simulation's
#'    output file
#'
#' @return NA
#' @export
#'
he_define_output_params <-
  function(
    model_env,
    output_dir = "output",
    model_run_id = NULL,
    infected_net_pen_output_file_name = "infected_net_pens.csv"
  ) {
    model_env$output_dir <- output_dir
    model_env$model_run_id <- model_run_id
    model_env$infected_net_pen_output_file_name <- infected_net_pen_output_file_name
  }
