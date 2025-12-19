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
#' @param days_dead_infectious number of days dead animals remain infectious
#' @param farm_to_farm scaling parameter for between-farm infection transmission
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
    days_dead_infectious = 2,
    farm_to_farm = 0.42,
    net_pen_to_net_pen = 0.052,
    vaccine_efficacy = 0
  ) {
    # TODO: Input validation, especially for index_infection_stage
    model_env$n_index_infected_min <- n_index_infected_min
    model_env$n_index_infected_mode <- n_index_infected_mode
    model_env$n_index_infected_max <- n_index_infected_max
    model_env$index_farm_id <- index_farm_id
    model_env$index_net_pen_ids <- index_net_pen_ids
    model_env$index_infection_stage <- index_infection_stage
    model_env$days_dead_infectious <- days_dead_infectious
    model_env$farm_to_farm <- farm_to_farm
    model_env$net_pen_to_net_pen <- net_pen_to_net_pen
    model_env$vaccine_efficacy <- vaccine_efficacy

    # Apply vaccine efficacy to clinically infected prop
    model_env$clinically_infected_prop <-
      clinically_infected_prop * (1 - vaccine_efficacy)
  }
