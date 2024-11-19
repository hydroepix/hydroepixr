#' Define simulation variables related to depopulation control
#'
#' @param environment environment in which simulation variables are stored and managed
#' @param control_functions functions which define infection control methods
#' @param prob_diagnosis probability of diagnosis given an infected individual is surveyed
#' @param mortality_increase_1 ?
#' @param mortality_increase_2 ?
#' @param time_between_visits_within_zone length of time between visits within a surveillance zone
#' @param protection_zone_duration length of time that an implemented protection zone lasts
#' @param past_days_for_dead_animal_surveillance ?
#' @param daily_farm_survey_limit maximum number of farms that can be surveyed in a day
#' @param output_surveillance_details logical indicating whether to output detailed surveillance information
#'
#' @return NA
#' @export
#'
he_define_surveillance_control_params <-
  function(environment,
           control_functions = c("SurvZone(label='SZ')"),
           # TODO: Is there a better way to provide these?
           prob_diagnosis = 1,
           # TODO: renaming of ProbSelDiag - OK?
           mortality_increase_1 = 2,
           mortality_increase_2 = 2,
           # TODO: how are these distinct? are they both necessary?
           #mortality_increase_zone = 1.5, # TODO: Commented out in HEoptions-trimmed.R - is it necessary?
           time_between_visits_within_zone = 4,
           # TODO: renaming of ZSurVisit - OK?
           protection_zone_duration = 50,
           # TODO: renaming of ZoneDuration - OK?
           past_days_for_dead_animal_surveillance = 7,
           # TODO: renaming of DaysSurDead - OK? shorten?,
           daily_farm_survey_limit = 2,
           # TODO: renaming of CapSurvey - OK?
           output_surveillance_details = FALSE # TODO: renaming of Detailed - OK?
           ) {
    environment$control_functions <- control_functions
    environment$prob_diagnosis <- prob_diagnosis
    environment$mortality_increase_1 <- mortality_increase_1
    environment$mortality_increase_2 <- mortality_increase_2
    environment$time_between_visits_within_zone <- time_between_visits_within_zone
    environment$protection_zone_duration <- protection_zone_duration
    environment$past_days_for_dead_animal_surveillance <- past_days_for_dead_animal_surveillance
    environment$daily_farm_survey_limit <- daily_farm_survey_limit
    environment$output_surveillance_details <- output_surveillance_details
    # TODO: Convert strings to functions

           }
