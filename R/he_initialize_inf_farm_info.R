#' Initialize data frame to store and track infected farm information
#'
#' @return empty data frame with appropriate column headers and types to store
#'    infected farm and netpen information
#' @export
#'
he_initialize_inf_farm_info <- function() {
  data.frame(
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
