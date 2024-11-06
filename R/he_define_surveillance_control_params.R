he_define_surveillance_control_params <-
  function(control_functions = c("SurvZone(label='SZ"), # TODO: Is there a better way to provide these?
           prob_diagnosis = 1, # TODO: renaming of ProbSelDiag - OK?
           mortality_increase_1 = 2,
           mortality_increase_2 = 2, # TODO: how are these distinct? are they both necessary?
           #mortality_increase_zone = 1.5, # TODO: Commented out in HEoptions-trimmed.R - is it necessary?
           time_between_visits_within_zone = 4, # TODO: renaming of ZSurVisit - OK?
           protection_zone_duration = 50, # TODO: renaming of ZoneDuration - OK?
           past_days_for_dead_animal_surveillance = 7, # TODO: renaming of DaysSurDead - OK? shorten?,
           daily_farm_survey_limit = 2, # TODO: renaming of CapSurvey - OK?
           output_surveillance_details = FALSE # TODO: renaming of Detailed - OK?
           ) {
    # TODO: Convert strings to functions

  }
