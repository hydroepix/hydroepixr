#' Initialize index infection and start tracking infected netpens
#'
#' @param infected_netpen_info data frame of infected netpen information
#' @param simulation_env environment containing simulation variables
#' @param n_index_infected_min a minimum number of animals to be infected
#' @param n_index_infected_mode a mode of animals to be infected
#' @param n_index_infected_max a maximum number of animals to be infected
#' @param species_info data frame of species information
#' @param netpen_info data frame of netpen information
#' @param index_netpen_ids netpen ids selected for index infection
#' @param index_infection_stage infection stage in which animals infected by the
#'    index infection should start
#' @param clinically_infected_prop the proportion of animals which will enter
#'    the clinical stage upon infection, which is the same as 1 minus the
#'    proportion of animals which will enter the subclinical stage upon
#'    infection
#'
#' @return infected netpen info data frame with infection information for
#'    the index netpen(s) appended
#' @export
#'
he_initialize_infection <- function(infected_netpen_info,
                                    simulation_env,
                                    n_index_infected_min,
                                    n_index_infected_mode,
                                    n_index_infected_max,
                                    species_info,
                                    netpen_info,
                                    index_netpen_ids,
                                    index_infection_stage,
                                    clinically_infected_prop = NULL) {
  # Calculate number of animals initially infected for each netpen infected
  n_animals_infected <- round(
    he_rpert(
      length(index_netpen_ids),
      n_index_infected_min,
      n_index_infected_mode,
      n_index_infected_max
    )
  )

  if (index_infection_stage == "subclinical-clinical split") {
    if (is.null(clinically_infected_prop)) {
      stop("Error: clinically_infected_prop must be provided if infection is initialized as a subclinical-clinical infection split.")
    }
    subclinical_clinical_split <-
      he_calculate_subclinical_clinical_infection_split(
        n_animals_infected,
        simulation_env$clinically_infected_prop
      )
    infected_netpen_info <- he_add_infected_netpen(infected_netpen_info,
                                           netpen_info,
                                           index_netpen_ids,
                                           n_infected_animals_by_stage = data.frame(
                                             n_latent = 0,
                                             n_subclinical = subclinical_clinical_split[1],
                                             n_clinical = subclinical_clinical_split[2]
                                           ),
                                           infection_origin = "index",
                                           simulation_day = 0)
    simulation_env$disease_stage_duration_matrices$latent_duration <-
      he_add_disease_stage_duration(simulation_env$latent_duration,
                                    species_info$latent_dur_freq[[1]],
                                    n_animals_to_distribute = 0)
    simulation_env$disease_stage_duration_matrices$subclinical_duration <-
      he_add_disease_stage_duration(simulation_env$subclinical_duration,
                                    species_info$subclinical_dur_freq[[1]],
                                    n_animals_to_distribute = subclinical_clinical_split[1])
    simulation_env$disease_stage_duration_matrices$clinical_duration <-
      he_add_disease_stage_duration(simulation_env$clinical_duration,
                                    species_info$clinical_dur_freq[[1]],
                                    n_animals_to_distribute = subclinical_clinical_split[2])
  } else if (index_infection_stage == "latent") {
    infected_netpen_info <- he_add_infected_netpen(infected_netpen_info,
                                           netpen_info,
                                           index_netpen_ids,
                                           n_infected_animals_by_stage = data.frame(
                                             n_latent = n_animals_infected,
                                             n_subclinical = 0,
                                             n_clinical = 0
                                           ),
                                           infection_origin = "index",
                                           simulation_day = 0)
    simulation_env$disease_stage_duration_matrices$latent_duration <-
      he_add_disease_stage_duration(simulation_env$latent_duration,
                                    species_info$latent_dur_freq[[1]],
                                    n_animals_to_distribute = n_animals_infected)
    simulation_env$disease_stage_duration_matrices$subclinical_duration <-
      he_add_disease_stage_duration(simulation_env$subclinical_duration,
                                    species_info$subclinical_dur_freq[[1]],
                                    n_animals_to_distribute = 0)
    simulation_env$disease_stage_duration_matrices$clinical_duration <-
      he_add_disease_stage_duration(simulation_env$clinical_duration,
                                    species_info$clinical_dur_freq[[1]],
                                    n_animals_to_distribute = 0)
  } else {
    stop("Invalid disease stage for index infection. Valid options are: 'latent' and 'subclinical-clinical split'")
  }
  # Output initial infection state to output file
  he_write_infected_netpen_output(infected_netpen_info,
                             simulation_env$infected_netpen_output_file_name,
                             simulation_env$output_dir)
  infected_netpen_info
}
