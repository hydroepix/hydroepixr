he_define_depopulation_control_params <-
  function(environment,
           culling_capacity = c(20000),
           cull_types = c(1:18),
           cull_farm_if_cage_infection = FALSE # TODO: renaming of cullAll? better renaming?
           ) {
   environment$culling_capacity <- culling_capacity
   environment$cull_types <- cull_types
   environment$cull_farm_if_cage_infection <-
     cull_farm_if_cage_infection
   }
