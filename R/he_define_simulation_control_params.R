#' Define simulation variables related to simulation control
#'
#' @param model_env environment in which simulation variables are stored and
#'   managed
#' @param n_simulations number of simulations (i.e., individual simulated epidemics) to be run
#' @param max_outbreak_length maximum length of a disease outbreak
#' @param random_seed random seed to use for the simulation
#' @param ignore_disease_status_input boolean indicating whether or not to
#'   ignore disease status input?
#' @param verbose boolean indicating whether verbose output should be generated
#'
#' @return NA
#' @export

he_define_simulation_control_params <-
  function(
    model_env,
    n_simulations = 10,
    max_outbreak_length = 365,
    random_seed = -10,
    ignore_disease_status_input = TRUE,
    verbose = FALSE
  ) {
    if (n_simulations < 10) {
      warning(
        "Due to the inclusion of randomness in the model, running a small number of simulations will provide a less representative model. It is recommended to run several simulations and aggregate their results."
      )
    }
    if (n_simulations > 1000) {
      warning(
        "Running a large number of simulations will take a large amount of time. It is recommended not to run more than 1000 simulations."
      )
    }
    if (max_outbreak_length > 720) {
      warning(
        "Simulations with a long max_outbreak_length will take a large amount of time to run and will not typically represent the length of time the animals will be in the water. Values of 720 days or less are recommended."
      )
    }
    model_env$n_simulations <- n_simulations
    model_env$max_outbreak_length <- max_outbreak_length
    model_env$random_seed <- random_seed
    model_env$ignore_disease_status_input <-
      ignore_disease_status_input
    model_env$verbose <- verbose
  }
