#' Define simulation variables related to spread control
#'
#' @param environment environment in which simulation variables are stored and
#'   managed
#' @param new_infection_functions functions which define infection spread
#' @param random_num_initially_infected_fish logical indicating whether to
#'   randomize the initial number of infected fish
#' @param intrafarm_disease_transmission_model defines which intrafarm disease
#'   transmission model is used, options are "binomial chain" and "reed-frost"
#' @param index_farm_function function which defines how the index farm is
#'   selected
#' @param index_farm_select arguments provided to the index_farm_function
#' @param index_direct ?
#' @param mort_threshold_for_first_investigation ?
#' @param mort_threshold_for_subsequent_investigation ?
#' @param case_fatality_prop proportion of the infected population that will die
#' @param days_dead_infectious number of days dead animals remain infectious
#' @param farm_to_farm rate of infection between farms
#' @param cage_to_cage rate of infection between cages
#' @param vaccine_efficacy proportion of the vaccinated population that will
#'   survive if infected?
#'
#' @return NA
#' @export
#'
he_define_spread_control_params <-
  function(environment,
           new_infection_functions = c("BCSpread()", "DBinf()"),
           # TODO: Is there a better way to provide these?
           random_num_initially_infected_fish = FALSE,
           intrafarm_disease_transmission_model = "binomial chain",
           index_farm_function = "selectIndexFarm",
           index_farm_select = list(species = 1:18),
           # TODO: argument provided to index_farm_function - better way to represent or name?
           index_direct = FALSE,
           mort_threshold_for_first_investigation = 0.00255,
           mort_threshold_for_subsequent_investigation = 0.00255,
           case_fatality_prop = 0.89,
           days_dead_infectious = 2,
           farm_to_farm = 0.42,
           cage_to_cage = 0.052,
           vaccine_efficacy = 0) {
    # TODO: Add check for valid options for intrafarm_disease_transmission_model
    environment$new_infection_functions <- new_infection_functions
    environment$random_num_initially_infected_fish <-
      random_num_initially_infected_fish
    environment$intrafarm_disease_transmission_model <-
      intrafarm_disease_transmission_model
    environment$index_farm_function <- index_farm_function
    environment$index_farm_select <- index_farm_select
    environment$index_direct <- index_direct
    environment$mort_threshold_for_first_investigation <-
      mort_threshold_for_first_investigation
    environment$mort_threshold_for_subsequent_investigation <-
      mort_threshold_for_subsequent_investigation
    environment$case_fatality_prop <- case_fatality_prop
    environment$days_dead_infectious <- days_dead_infectious
    environment$farm_to_farm <- farm_to_farm
    environment$cage_to_cage <- cage_to_cage
    environment$vaccine_efficacy <- vaccine_efficacy
  }
