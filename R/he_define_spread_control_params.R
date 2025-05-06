#' Define simulation variables related to spread control
#'
#' @param model_env environment in which simulation variables are stored and
#'   managed
#' @param num_index_infected_min minimum number of fish initially infected,
#'    to be calculated from a PERT distribution
#' @param num_index_infected_mode mode of fish initially infected, to be
#'    calculated from a PERT distribution
#' @param num_index_infected_max maximum number of fish initially infected, to
#'    be calculated from a PERT distribution
#' @param index_netpen_ids identifiers of the index netpens
#' @param index_farm_id identifier of the index farm
#' @param index_direct logical indicating whether the index farm gets infected
#'    directly, as opposed to indirectly
#' @param case_fatality_prop proportion of the infected population that will die
#' @param days_dead_infectious number of days dead animals remain infectious
#' @param farm_to_farm scaling parameter for between-farm infection transmission
#' @param netpen_to_netpen daily probability for between-netpen infection
#'    transmission within a farm
#' @param vaccine_efficacy product of the manufacturer-reported vaccine efficacy
#'    and the population coverage of the vaccine
#'
#' @return NA
#' @export
#'
he_define_spread_control_params <-
  function(model_env,
           num_index_infected_min = 1,
           num_index_infected_mode = 10,
           num_index_infected_max = 100,
           index_netpen_ids = NULL,
           index_farm_id = NULL,
           index_direct = TRUE,
           case_fatality_prop = 0.89,
           days_dead_infectious = 2,
           farm_to_farm = 0.42,
           netpen_to_netpen = 0.052,
           vaccine_efficacy = 0) {
    model_env$num_index_infected_min <- num_index_infected_min
    model_env$num_index_infected_mode <- num_index_infected_mode
    model_env$num_index_infected_max <- num_index_infected_max
    model_env$index_netpen_ids <- index_netpen_ids
    model_env$index_farm_id <- index_farm_id
    model_env$case_fatality_prop <- case_fatality_prop
    model_env$days_dead_infectious <- days_dead_infectious
    model_env$farm_to_farm <- farm_to_farm
    model_env$netpen_to_netpen <- netpen_to_netpen
    model_env$vaccine_efficacy <- vaccine_efficacy
  }
