#' Define simulation variables related to spread control
#'
#' @param model_env environment in which simulation variables are stored and
#'   managed
#' @param n_index_infected_min minimum number of fish initially infected,
#'    to be calculated from a PERT distribution
#' @param n_index_infected_mode mode of fish initially infected, to be
#'    calculated from a PERT distribution
#' @param n_index_infected_max maximum number of fish initially infected, to
#'    be calculated from a PERT distribution
#' @param index_net_pen_ids identifiers of the index net pens
#' @param index_farm_id identifier of the index farm
#' @param index_infection_stage indication of the infection stage in which the
#'    index infection will begin, valid options are "latent" and
#'    "subclinical-clinical split"
#' @param clinically_infected_prop the proportion of animals which will enter
#'    the clinical stage upon infection, which is the same as 1 minus the
#'    proportion of animals which will enter the subclinical stage upon
#'    infection
#' @param net_pen_to_net_pen daily probability of infection transmission between net pens within a farm
#' @param vaccine_efficacy product of the manufacturer-reported vaccine efficacy
#'    and the population coverage of the vaccine
#'
#' @return NA
#' @export
#'
he_define_spread_control_params <-
  function(
    model_env,
    n_index_infected_min = 1,
    n_index_infected_mode = 10,
    n_index_infected_max = 100,
    index_net_pen_ids = NULL,
    index_farm_id = NULL,
    index_infection_stage = "subclinical-clinical split",
    clinically_infected_prop = 0.89,
    net_pen_to_net_pen = 0.052,
    vaccine_efficacy = 0
  ) {
    # Input validation
    # all n_index_infected variables must be greater than 0
    if (!is.numeric(n_index_infected_min) | n_index_infected_min < 1) {
      stop(
        "Error: n_index_infected_min value must be a numeric value greater than 0"
      )
    }
    if (!is.numeric(n_index_infected_mode) | n_index_infected_mode < 1) {
      stop(
        "Error: n_index_infected_mode value must be a numeric value greater than 0"
      )
    }
    if (!is.numeric(n_index_infected_max) | n_index_infected_max < 1) {
      stop(
        "Error: n_index_infected_max value must be a numeric value greater than 0"
      )
    }
    # n_index_infected_min must be the smallest value (or tied), n_index_infected_max must be the largest value (or tied)
    if (
      n_index_infected_min > n_index_infected_mode |
        n_index_infected_min > n_index_infected_max
    ) {
      stop(
        "Error: n_index_infected_min value must be smaller than n_index_infected_mode and n_index_infected_max"
      )
    }
    if (n_index_infected_mode > n_index_infected_max) {
      stop(
        "Error: n_index_infected_mode must be smaller than n_index_infected_max"
      )
    }
    # index_infection_stage
    if (
      index_infection_stage != "subclinical-clinical split" &
        index_infection_stage != "latent"
    ) {
      stop(
        paste0(
          "Error: ",
          index_infection_stage,
          " is not a valid value for index_infection_stage. Valid values are 'subclinical-clinical split' or 'latent'"
        )
      )
    }
    # clinically_infected_prop
    if (
      !is.numeric(clinically_infected_prop) |
        clinically_infected_prop < 0 |
        clinically_infected_prop > 1
    ) {
      stop(
        "Error: clinically_infected_prop value must be a numeric value between 0 and 1"
      )
    }
    if (clinically_infected_prop < 0.05 | clinically_infected_prop > 0.9) {
      warning(
        "Warning: clinically_infected_prop value should typically be between 0.05 and 0.9"
      )
    }
    # net_pen_to_net_pen
    if (
      !is.numeric(net_pen_to_net_pen) |
        net_pen_to_net_pen < 0 |
        net_pen_to_net_pen > 1
    ) {
      stop(
        "Error: net_pen_to_net_pen value must be a numeric value between 0 and 1"
      )
    }
    if (net_pen_to_net_pen < 0.01 | net_pen_to_net_pen > 0.1) {
      warning(
        "Warning: net_pen_to_net_pen value should typically be between 0.01 and 0.1"
      )
    }
    # vaccine_efficacy = 0
    if (
      !is.numeric(vaccine_efficacy) |
        vaccine_efficacy < 0 |
        vaccine_efficacy > 1
    ) {
      stop(
        "Error: vaccine_efficacy value must be a numeric value between 0 and 1"
      )
    }
    if (vaccine_efficacy > 0.6) {
      warning(
        "Warning: vaccine_efficacy values should typically not be above 0.6"
      )
    }
    # TODO: Eventually add days_dead_infectious and farm_to_farm
    # model_env$days_dead_infectious <- days_dead_infectious
    # model_env$farm_to_farm <- farm_to_farm

    # Assign variables to model environment
    model_env$n_index_infected_min <- n_index_infected_min
    model_env$n_index_infected_mode <- n_index_infected_mode
    model_env$n_index_infected_max <- n_index_infected_max
    model_env$index_farm_id <- index_farm_id
    model_env$index_net_pen_ids <- index_net_pen_ids
    model_env$index_infection_stage <- index_infection_stage
    model_env$net_pen_to_net_pen <- net_pen_to_net_pen
    model_env$vaccine_efficacy <- vaccine_efficacy

    # Apply vaccine efficacy to clinically infected prop
    model_env$clinically_infected_prop <-
      clinically_infected_prop * (1 - vaccine_efficacy)
  }
