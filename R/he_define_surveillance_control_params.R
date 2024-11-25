#' Define simulation variables related to surveillance control
#'
#' @param environment environment in which simulation variables are stored and
#'   managed
#' @param control_functions functions which define infection control methods
#' @param prob_diagnosis probability of diagnosis given an infected individual
#'   is surveyed
#' @param mort_increase_factor_for_first_investigation factor of increase in
#'   mortality required to trigger a first investigation
#' @param mort_increase_factor_for_subsequent_investigation factor of increase
#'   in mortality required to trigger a subsequent investigation
#' @param time_between_survey_days length of time between visits within a
#'   surveillance zone in days
#' @param max_duration_surveillance_zone_days length of time that an implemented protection
#'   zone lasts in days
#' @param past_days_for_dead_animal_surveillance ?
#' @param daily_farm_survey_limit maximum number of farms that can be surveyed
#'   in a day in each bay
#' @param output_surveillance_details logical indicating whether to output
#'   detailed surveillance information
#'
#' @return NA
#' @export
#'
he_define_surveillance_control_params <-
  function(environment,
           control_functions = c("SurvZone(label='SZ')"),
           # TODO: Is there a better way to provide these?
           prob_diagnosis = 1,
           mort_increase_factor_for_first_investigation = 2,
           mort_increase_factor_for_subsequent_investigation = 2,
           time_between_survey_days = 4,
           max_duration_surveillance_zone_days = 50,
           past_days_for_dead_animal_surveillance = 7,
           daily_farm_survey_limit = 2,
           output_surveillance_details = FALSE
           ) {
    environment$control_functions <- control_functions
    environment$prob_diagnosis <- prob_diagnosis
    environment$mort_increase_factor_for_first_investigation <- mort_increase_factor_for_first_investigation
    environment$mort_increase_factor_for_subsequent_investigation <- mort_increase_factor_for_subsequent_investigation
    environment$time_between_survey_days <- time_between_survey_days
    environment$max_duration_surveillance_zone_days <- max_duration_surveillance_zone_days
    environment$past_days_for_dead_animal_surveillance <- past_days_for_dead_animal_surveillance
    environment$daily_farm_survey_limit <- daily_farm_survey_limit
    environment$output_surveillance_details <- output_surveillance_details

           }
