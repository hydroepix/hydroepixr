#' Define simulation variables related to spread control
#'
#' @param environment environment in which simulation variables are stored and
#'   managed
#' @param random_num_initially_infected_fish logical indicating whether to
#'   randomize the initial number of infected fish
#' @param intrafarm_disease_transmission_model defines which intrafarm disease
#'   transmission model is used, options are "binomial chain" and "reed-frost"
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
  function(environment,
           random_num_initially_infected_fish = FALSE,
           intrafarm_disease_transmission_model = "binomial chain",
           index_farm_id = NULL,
           # TODO: argument provided to index_farm_function - better way to represent or name?
           index_direct = TRUE, # note that all fish and farms are direct?
           case_fatality_prop = 0.89,
           days_dead_infectious = 2,
           farm_to_farm = 0.42,
           netpen_to_netpen = 0.052,
           vaccine_efficacy = 0) {
    # TODO: Add check for valid options for intrafarm_disease_transmission_model
    environment$random_num_initially_infected_fish <-
      random_num_initially_infected_fish
    environment$intrafarm_disease_transmission_model <-
      intrafarm_disease_transmission_model
    environment$index_farm_id <- index_farm_id
    environment$case_fatality_prop <- case_fatality_prop
    environment$days_dead_infectious <- days_dead_infectious
    environment$farm_to_farm <- farm_to_farm
    environment$netpen_to_netpen <- netpen_to_netpen
    environment$vaccine_efficacy <- vaccine_efficacy
  }
