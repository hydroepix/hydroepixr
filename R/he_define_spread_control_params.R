#' Define simulation variables related to spread control
#'
#' @param model_env environment in which simulation variables are stored and
#'   managed
#' @param random_num_initially_infected_fish logical indicating whether to
#'   randomize the initial number of infected fish
#' @param intrafarm_disease_transmission_model defines which intrafarm disease
#'   transmission model is used, options are "binomial chain" and "reed-frost"
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
           random_num_initially_infected_fish = FALSE,
           intrafarm_disease_transmission_model = "binomial chain",
           index_netpen_ids = NULL,
           index_farm_id = NULL,
           index_direct = TRUE, # TODO: Is this indicating whether the initial infection was direct?
           case_fatality_prop = 0.89,
           days_dead_infectious = 2,
           farm_to_farm = 0.42,
           netpen_to_netpen = 0.052,
           vaccine_efficacy = 0) {
    # TODO: Confirm whether intrafarm_disease_transmission_model is relevant
    # TODO: Add check for valid options for intrafarm_disease_transmission_model?
    model_env$random_num_initially_infected_fish <-
      random_num_initially_infected_fish
    model_env$intrafarm_disease_transmission_model <-
      intrafarm_disease_transmission_model
    model_env$index_netpen_ids <- index_netpen_ids
    model_env$index_farm_id <- index_farm_id
    model_env$case_fatality_prop <- case_fatality_prop
    model_env$days_dead_infectious <- days_dead_infectious
    model_env$farm_to_farm <- farm_to_farm
    model_env$netpen_to_netpen <- netpen_to_netpen
    model_env$vaccine_efficacy <- vaccine_efficacy
  }
