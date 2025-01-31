he_initialize_mort_info <- function(environment) {
  environment$mort_info <- data.frame(
    simulation = integer(),
    day = integer(),
    inf_farm_id = integer(),
    inf_netpen_id = integer(),
    mort = double(), # TODO: Clarify what is stored here and fix name
  )
}
