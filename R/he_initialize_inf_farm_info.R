#' Initialize matrix to store and track infected farm information
#'
#' @param farm_info
#'
#' @return NA
#' @export
#'
he_initialize_inf_farm_info <- function(farm_info) {
  environment$inf_farm_info <- data.frame(
    farm_id = integer(),
    species_id = integer(),
    susceptible = integer(),
    latent = integer(),
    subclinical = integer(),
    clinical = integer(),
    immune = integer(),
    total = integer(), #???
    infection_status = integer(), #???
    #"p", # Reed-Frost probability?
    latent_duration = double(), # Length of time in latent stage?
    subclinical_duration = double(), # Length of time in subclinical stage?
    clinical_time = double(), # time the herd showed clinical signs of infection
    time_of_diagnosis = double(),
    diagnosed = logical(),
    infected_by_direct_contact = logical(),
    time_infected = double(), # appears to refer to the timestep of infection
    vaccinated = logical()#,
    #"TLastAnCli19" #???
  )
}
