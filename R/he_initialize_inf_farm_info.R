he_initialize_inf_farm_info <- function() {
  inf_farm_cols <- c("susceptible",
                     "latent",
                     "subclinical",
                     "clinical",
                     "immune",
                     "total", #???
                     "status", #???
                     "id", # Which id is this? farm or netpen?
                     "p", # Reed-Frost probability?
                     "latent_duration", # Length of time in latent stage?
                     "subclinical_duration", # Length of time in subclinical stage?
                     "clinical_time", # time the herd showed clinical signs of infection
                     "time_of_diagnosis",
                     "diagnosed",
                     "infected_by_direct_contact",
                     "time_infected", # is this a duration or a time?
                     "species",
                     "vaccinated",
                     "TLastAnCli19") #???

  farms <- matrix(numeric(0), ncol = length(inf_farm_cols))
  dead_animal_matrix <- matrix(numeric(0), ncol = days_dead)
  # TODO: This seems to relate to surveillance - figure out how?
  dead_animal_matrix_sur <- matrix(numeric(0), ncol = days_sur_dead)

  # What's the difference between iteration and day here? What is mort?
  mort_matrix_cols <- c("iteration",
                     "inf_farm_id",
                     "inf_netpen_id",
                     "mort",
                     "day")
  mort_matrix <- matrix(numeric(0), ncol = 5)
  deleted_herd_matrix <- matrix(numeric(0), length(inf_farm_cols))


}
