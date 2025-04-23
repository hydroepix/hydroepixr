he_simulate_day <- function(inf_farm_info,
                            simulation_env,
                            species_info) {

  # TODO: Future feature, update farm production time and whether farm is active

  # Check there are infected netpens to cause infection spread
  if (nrow(inf_farm_info) > 0) {
    # Calculate probability of within-netpen infection
    prob_within_netpen_infection <-
      he_calculate_within_netpen_infection_prob(inf_farm_info,
                                                vaccine_efficacy = 0)
    # TODO: Sampling should only be done for netpens with remaining susceptible
    # animals - nrow(inf_farm_info) yields all infected farms
    # Does this generate NAs because of sampling a 0 from farms with no remaining
    # susceptible animals?
    num_newly_infected <- rbinom(nrow(inf_farm_info),
                                 inf_farm_info$susceptible,
                                 prob_within_netpen_infection)
    # Add and remove fish from different disease stages according to new
    # infections and the number of fish which have reached their duration in
    # their disease stage
    disease_stage_counts <-
      inf_farm_info[c("susceptible", "latent", "subclinical", "clinical", "immune")]
    inf_farm_info[c("susceptible", "latent", "subclinical", "clinical", "immune")] <-
      he_update_disease_stage_counts(disease_stage_counts,
                                     simulation_env$disease_stage_duration_matrices,
                                     num_newly_infected)
    # Update disease stage duration matrices for animals entering a new stage
    num_animals_transitioning_by_stage <-
      lapply(simulation_env$disease_stage_duration_matrices,
             FUN = \(matrix) matrix[, 1])
    disease_stage_distributions <-
      species_info[c("latent_dur_freq",
                     "subclinical_dur_freq",
                     "clinical_dur_freq")]

    simulation_env$disease_stage_duration_matrices <-
      he_update_disease_stage_duration_matrix(
        simulation_env$disease_stage_duration_matrices,
        disease_stage_distribution = disease_stage_distributions,
        num_animals_to_distribute = num_animals_transitioning_by_stage
        )

    # TODO: Update overall netpen infection statuses
    #inf_farm_info <- he_update_netpen_infection_status()

    # Generate output for a day
    # he_write_inf_netpen_output(simulation_env$simulation_day,
    #                            inf_farm_info)

    simulation_env$simulation_day <- simulation_env$simulation_day + 1
  } else {
    stop("No remaining infected netpens. Simulation should have terminated.")
  }
}
