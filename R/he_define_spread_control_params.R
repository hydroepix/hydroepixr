#' Define simulation variables related to depopulation control
#'
#' @param environment environment in which simulation variables are stored and managed
#' @param new_infection_functions functions which define infection spread
#' @param t_stochastic ?
#' @param rf_stochastic ?
#' @param index_farm_function function which defines how the index farm is selected
#' @param index_farm_select arguments provided to the index_farm_function
#' @param index_direct ?
#' @param first_det_para_1 ?
#' @param first_det_para_2 ?
#' @param case_fatality proportion of the infected population that will die
#' @param days_dead_infectious number of days dead animals remain infectious
#' @param farm_to_farm rate of infection between farms
#' @param cage_to_cage rate of infection between cages
#' @param vaccine_efficacy proportion of the vaccinated population that will survive if infected?
#'
#' @return NA
#' @export
#'
he_define_spread_control_params <-
  function(environment,
           new_infection_functions = c("BCSpread()", "DBinf()"),
           # TODO: Is there a better way to provide these?
           t_stochastic = FALSE,
           # TODO: options are stochastic or just 1 - rework to better represent this choice?
           rf_stochastic = FALSE,
           # TODO: options are binomial chain model or Reed-Frost model - rework to better represent this choice?
           index_farm_function = "selectIndexFarm",
           index_farm_select = list(farm_type = 1:18),
           # TODO: argument provided to index_farm_function - better way to represent or name?
           index_direct = FALSE,
           first_det_para_1 = 0.00255,
           first_det_para_2 = 0.00255,
           # TODO: How are these distinct? Are they both necessary?
           case_fatality = 0.89,
           days_dead_infectious = 2,
           farm_to_farm = 0.42,
           cage_to_cage = 0.052,
           vaccine_efficacy = 0) {
    environment$new_infection_functions <- new_infection_functions
    environment$t_stochastic <- t_stochastic
    environment$rf_stochastic <- rf_stochastic
    environment$index_farm_function <- index_farm_function
    environment$index_farm_select <- index_farm_select
    environment$index_direct <- index_direct
    environment$first_det_para_1 <- first_det_para_1
    environment$first_det_para_2 <- first_det_para_2
    environment$case_fatality <- case_fatality
    environment$days_dead_infectious <- days_dead_infectious
    environment$farm_to_farm <- farm_to_farm
    environment$cage_to_cage <- cage_to_cage
    environment$vaccine_efficacy <- vaccine_efficacy
    # TODO: Convert strings to functions?
  }
