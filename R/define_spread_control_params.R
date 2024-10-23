define_spread_control_params <-
  function(new_infection_functions = c("BCSpread()", "DBinf()"),
           t_stochastic = FALSE,
           rf_stochastic = FALSE,
           index_herd_function = "selectIndexHerd",
           index_herd_select = list(herd_type = 1:18),
           index_direct = FALSE,
           first_det_para_1 = 0.00255,
           first_det_para_2 = 0.00255,
           subclinical_infection_risk = 0.1,
           case_fatality = 0.89,
           days_dead_infectious = 2,
           dead_impact = 1,
           impact_dead_time = 1 #??
           ) {

  }
