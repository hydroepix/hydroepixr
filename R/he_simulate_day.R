#' Simulate a day of infection transmission dynamics
#'
#' @param infected_net_pen_info data frame of information on infected net pens
#' @param simulation_env environment containing simulation variables
#' @param simulation_day day of simulation that infection is occurring
#' @param species_info matrix of species information
#' @param verbose a logical value indicating whether to provide additional output
#'    messages on model running progress
#'
#' @return updated infected net pen info data frame
#' @export
#' @importFrom utils head
#' @importFrom stats rbinom
#'
he_simulate_day <- function(
  infected_net_pen_info,
  simulation_env,
  simulation_day,
  species_info,
  verbose = FALSE
) {
  # TODO: Future feature, update farm production time and whether farm is active
  # Consider whether this is a farm-level variable or a net pen-level variable

  # Check there are infected net pens to cause infection spread
  if (nrow(infected_net_pen_info) > 0) {
    # Calculate probability of within-net pen infection
    if (verbose) {
      message(
        "Calculating newly infected animals from within-net pen transmission..."
      )
    }
    # TODO: Update this calculation to only be applied to the subset of net pens
    # which have active infections?
    prob_within_net_pen_infection <-
      he_calculate_within_net_pen_infection_prob(infected_net_pen_info)

    n_newly_infected <- stats::rbinom(
      nrow(infected_net_pen_info),
      infected_net_pen_info$n_susceptible,
      prob_within_net_pen_infection
    )
    if (verbose) {
      message(paste0("Newly infected animals: ", n_newly_infected))
    }

    # Update the day represented by the current stage of the infected net pen info
    infected_net_pen_info["simulation_day"] <- simulation_day

    # Add and remove fish from different disease stages according to new
    # infections and the number of fish which have reached their duration in
    # their current disease stage
    disease_stage_counts <-
      infected_net_pen_info[c(
        "n_susceptible",
        "n_latent",
        "n_subclinical",
        "n_clinical",
        "n_recovered",
        "n_dead"
      )]
    infected_net_pen_info[c(
      "n_susceptible",
      "n_latent",
      "n_subclinical",
      "n_clinical",
      "n_recovered",
      "n_dead"
    )] <-
      he_update_disease_stage_counts(
        disease_stage_counts,
        simulation_env$disease_stage_duration_matrices,
        n_newly_infected,
        simulation_env$clinically_infected_prop
      )

    # Update disease stage duration matrices for animals entering a new stage
    n_latent_out <- simulation_env$disease_stage_duration_matrices$latent_duration[,
      1,
      drop = FALSE
    ]
    subclinical_clinical_split <- he_calculate_subclinical_clinical_infection_split(
      n_transitioning = n_latent_out,
      clinically_infected_prop = simulation_env$clinically_infected_prop
    )
    n_animals_transitioning_by_stage <-
      c(
        n_latent_in = n_newly_infected,
        n_subclinical_in = subclinical_clinical_split[1],
        n_clinical_in = subclinical_clinical_split[2]
      )

    disease_stage_distributions <-
      species_info[c(
        "latent_dur_freq",
        "subclinical_dur_freq",
        "clinical_dur_freq"
      )]
    simulation_env$disease_stage_duration_matrices <-
      he_update_disease_stage_duration_matrix(
        simulation_env$disease_stage_duration_matrices,
        disease_stage_distribution = disease_stage_distributions,
        n_animals_to_distribute = n_animals_transitioning_by_stage
      )

    # Generate output for a day
    he_write_infected_net_pen_output(
      infected_net_pen_info,
      simulation_env$infected_net_pen_output_file_name,
      simulation_env$output_dir
    )

    infected_net_pen_info
  } else {
    stop("No remaining infected net pens. Simulation should have terminated.")
  }
}
