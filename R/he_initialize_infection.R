#' Initialize index infection and start tracking infected farms
#'
#' @param inf_farm_info data frame of infected farm information
#' @param simulation_env environment containing simulation variables
#' @param num_index_infected_min a minimum number of animals to be infected
#' @param num_index_infected_mode a mode of animals to be infected
#' @param num_index_infected_max a maximum number of animals to be infected
#' @param species_info data frame of species information
#' @param farm_info data frame of farm information
#' @param index_netpen_ids netpen ids selected for index infection
#' @param index_infection_stage infection stage in which animals infected by the
#'    index infection should start
#'
#' @return infected farm info data frame with infection information for
#'    the index farm appended
#' @export
#'
he_initialize_infection <- function(inf_farm_info,
                                    simulation_env,
                                    num_index_infected_min,
                                    num_index_infected_mode,
                                    num_index_infected_max,
                                    species_info,
                                    farm_info,
                                    index_netpen_ids,
                                    index_infection_stage = "subclinical") {
  # Calculate number of animals initially infected for each netpen infected
  n_animals_infected <- round(
    he_rpert(
      length(index_netpen_ids),
      num_index_infected_min,
      num_index_infected_mode,
      num_index_infected_max
    )
  )

  if (index_infection_stage == "subclinical") {
    inf_farm_info <- he_add_infected_netpen(inf_farm_info,
                                         farm_info,
                                         index_netpen_ids,
                                         num_inf_animals_by_stage = data.frame(
                                           n_latent = 0,
                                           n_subclinical = n_animals_infected,
                                           n_clinical = 0
                                         ),
                                         infection_origin = "index",
                                         simulation_day = 0)
    simulation_env$disease_stage_duration_matrices$latent_duration <-
      he_add_disease_stage_duration(simulation_env$latent_duration,
                                    species_info$latent_dur_freq[[1]],
                                    num_animals_to_distribute = 0)
    simulation_env$disease_stage_duration_matrices$subclinical_duration <-
      he_add_disease_stage_duration(simulation_env$subclinical_duration,
                                    species_info$subclinical_dur_freq[[1]],
                                    num_animals_to_distribute = n_animals_infected)
  } else if (index_infection_stage == "latent") {
    inf_farm_info <- he_add_infected_netpen(inf_farm_info,
                                         farm_info,
                                         index_netpen_ids,
                                         num_inf_animals_by_stage = data.frame(
                                           n_latent = n_animals_infected,
                                           n_subclinical = 0,
                                           n_clinical = 0
                                         ),
                                         infection_origin = "index",
                                         simulation_day = 0)
    simulation_env$disease_stage_duration_matrices$latent_duration <-
      he_add_disease_stage_duration(simulation_env$latent_duration,
                                    species_info$latent_dur_freq[[1]],
                                    num_animals_to_distribute = n_animals_infected)
    simulation_env$disease_stage_duration_matrices$subclinical_duration <-
      he_add_disease_stage_duration(simulation_env$subclinical_duration,
                                    species_info$subclinical_dur_freq[[1]],
                                    num_animals_to_distribute = 0)
  } else {
    stop("Invalid disease stage for index farm infection.")
  }
  simulation_env$disease_stage_duration_matrices$clinical_duration <-
    he_add_disease_stage_duration(simulation_env$clinical_duration,
                                  species_info$clinical_dur_freq[[1]],
                                  num_animals_to_distribute = 0)
  # Output initial infection state to output file
  he_write_inf_netpen_output(inf_farm_info,
                             simulation_env$inf_netpen_output_file_name,
                             simulation_env$output_dir)
  inf_farm_info
}
