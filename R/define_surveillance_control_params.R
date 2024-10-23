define_surveillance_control_params <-
  function(delay_times = expression(round(rpert(n, 1, 2, 4))), # TODO: Can we reformat this default? Do we need to?
           prob_diagnosis = 1, # TODO: reconsider variable naming?
           mortality_increase_1 = 2,
           mortality_increase_2 = 2, # TODO: how are these distinct? are they both necessary?
           time_between_visits_within_zone = 4, # TODO: renaming of ZSurVisit - OK?
           protection_zone_duration = 50,
           past_days_for_dead_animal_surveillance = 7, # TODO: renaming of DaysSurDead - shorten?,
           daily_farm_survey_limit = 2, # TODO: renaming of CapSurvey - OK?
           output_surveillance_details = FALSE) {

  }
