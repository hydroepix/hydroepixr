#' Define simulation variables related to simulation control
#'
#' @param model_env environment in which simulation variables are stored and
#'   managed
#' @param n_simulations number of simulations (i.e., individual simulated epidemics) to be run
#' @param run_id identifier for this run of the model (or the run of the simulation??)
#' @param max_outbreak_length maximum length of a disease outbreak
#' @param random_seed random seed to use for the simulation
#' @param ignore_disease_status_input boolean indicating whether or not to
#'   ignore disease status input?
#' @param verbose boolean indicating whether verbose output should be generated
#'
#' @return NA
#' @export

he_define_simulation_control_params <-
  function(model_env,
           n_simulations = 10,
           run_id = NULL,
           max_outbreak_length = 365,
           random_seed = -10,
           ignore_disease_status_input = TRUE,
           verbose = FALSE) {
    model_env$n_simulations <- n_simulations
    # TODO: Add default run id?
    model_env$run_id <- run_id
    model_env$max_outbreak_length <- max_outbreak_length
    model_env$random_seed <- random_seed
    model_env$ignore_disease_status_input <-
      ignore_disease_status_input
    model_env$verbose <- verbose
  }
