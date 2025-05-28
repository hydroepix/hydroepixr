#' Simulate a day of infection transmission dynamics
#'
#' @param infected_netpen_info data frame of information on infected netpens
#' @param simulation_env environment containing simulation variables
#' @param simulation_day day of simulation that infection is occurring
#' @param species_info matrix of species information
#' @param verbose a logical value indicating whether to provide additional output
#'    messages on model running progress
#'
#' @return updated infected netpen info data frame
#' @export
#' @importFrom utils head
#' @importFrom stats rbinom
#'
he_simulate_day <- function(infected_netpen_info,
                            simulation_env,
                            simulation_day,
                            species_info,
                            verbose = FALSE) {

  # TODO: Future feature, update farm production time and whether farm is active
  # Consider whether this is a farm-level variable or a netpen-level variable

  # Check there are infected netpens to cause infection spread
  if (nrow(infected_netpen_info) > 0) {
    # Calculate probability of within-netpen infection
    if (verbose) {
      message("Calculating newly infected animals from within-netpen transmission...")
    }
    # TODO: Update this calculation to only be applied to the subset of netpens
    # which have active infections?
    prob_within_netpen_infection <-
      he_calculate_within_netpen_infection_prob(infected_netpen_info,
                                                vaccine_efficacy = 0)

    n_newly_infected <- stats::rbinom(
      nrow(infected_netpen_info),
      infected_netpen_info$n_susceptible,
      prob_within_netpen_infection
    )
    if (verbose) {
      message(paste0("Newly infected animals: ",
                     n_newly_infected))
    }

    # Update the day represented by the current stage of the infected netpen info
    infected_netpen_info["simulation_day"] <- simulation_day

    # Add and remove fish from different disease stages according to new
    # infections and the number of fish which have reached their duration in
    # their current disease stage
    disease_stage_counts <-
      infected_netpen_info[c("n_susceptible",
                      "n_latent",
                      "n_subclinical",
                      "n_clinical",
                      "n_recovered",
                      "n_dead")]
    infected_netpen_info[c("n_susceptible",
                    "n_latent",
                    "n_subclinical",
                    "n_clinical",
                    "n_recovered",
                    "n_dead")] <-
      he_update_disease_stage_counts(
        disease_stage_counts,
        simulation_env$disease_stage_duration_matrices,
        n_newly_infected
      )

    # Update disease stage duration matrices for animals entering a new stage
    n_animals_transitioning_by_stage <-
      c(n_newly_infected,
        lapply(
          utils::head(simulation_env$disease_stage_duration_matrices,-1),
          FUN = \(matrix) matrix[, 1]
        ))

    disease_stage_distributions <-
      species_info[c("latent_dur_freq",
                     "subclinical_dur_freq",
                     "clinical_dur_freq")]

    simulation_env$disease_stage_duration_matrices <-
      he_update_disease_stage_duration_matrix(
        simulation_env$disease_stage_duration_matrices,
        disease_stage_distribution = disease_stage_distributions,
        n_animals_to_distribute = n_animals_transitioning_by_stage
        )

    # Generate output for a day
    he_write_infected_netpen_output(
      infected_netpen_info,
      simulation_env$infected_netpen_output_file_name,
      simulation_env$output_dir
    )

    infected_netpen_info
  } else {
    stop("No remaining infected netpens. Simulation should have terminated.")
  }
}
