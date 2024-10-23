define_spread_control_params <-
  function(new_infection_functions = c("BCSpread()", "DBinf()"),
           t_stochastic = FALSE, # TODO: options are stochastic or just 1 - rework to better represent this choice?
           rf_stochastic = FALSE, # TODO: options are binomial chain model or Reed-Frost model - rework to better represent this choice?
           index_herd_function = "selectIndexHerd",
           index_herd_select = list(herd_type = 1:18), # TODO: argument provided to index_herd_function - better way to represent or name?
           index_direct = FALSE,
           first_det_para_1 = 0.00255,
           first_det_para_2 = 0.00255, # TODO: How are these distinct? Are they both necessary?
           case_fatality = 0.89,
           days_dead_infectious = 2,
           dead_impact = 1,
           impact_dead_time = 1, #??
           farm_to_farm = 0.42,
           cage_to_cage = 0.052,
           vaccine_efficacy = 0
           ) {

  }
