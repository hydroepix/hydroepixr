#' Define simulation variables related to simulation control
#'
#' @param environment environment in which simulation variables are stored and
#'   managed
#' @param n number of iterations?
#' @param run_id identifier for this run of the model
#' @param step_in_file filename for step-wise information?
#' @param max_outbreak_length maximum length of a disease outbreak
#' @param random_seed random seed to use for the simulation
#' @param ignore_disease_status_input boolean indicating whether or not to
#'   ignore disease status input?
#' @param verbose boolean indicating whether verbose output should be generated
#' @param summary_function text indicating which summary function should be used
#'   to summarize simulation outputs
#'
#' @return NA
#' @export

he_define_simulation_control_params <-
  function(environment,
           n = 1,
           run_id = NULL,
           step_in_file = NULL,
           max_outbreak_length = 365,
           random_seed = -10,
           ignore_disease_status_input = TRUE,
           verbose = FALSE,
           summary_function = "HEsum") {
    environment$n <- n
    environment$run_id <- run_id
    environment$step_in_file <- step_in_file
    environment$max_outbreak_length <- max_outbreak_length
    environment$random_seed <- random_seed
    environment$ignore_disease_status_input <-
      ignore_disease_status_input
    environment$verbose <- verbose
    environment$summary_function <- summary_function
  }
