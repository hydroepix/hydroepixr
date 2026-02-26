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
#'
#' @import foreach
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
  # Check there are infected net pens to cause infection spread
  if (nrow(infected_net_pen_info) > 0) {
    # Calculate probability of between-net pen infection
    # NOTE: This will have to be done for each farm
    # once between farm infection is implemented
    if (verbose) {
      message(
        "Calculating newly infected animals from between-net pen transmission..."
      )
    }
    newly_infected_net_pens <- c()
    foreach::foreach(farm_id = unique(simulation_env$net_pen_info$farm_id)) %do%
      {
        prob_between_net_pen_infection <-
          he_calculate_between_net_pen_infection_prob(
            simulation_env$net_pen_to_net_pen,
            infected_net_pen_info |> dplyr::filter(farm_id == !!farm_id)
          )
        # Determine number of susceptible net pens
        susceptible_net_pens <- he_retrieve_susceptible_net_pens(
          farm_id,
          simulation_env$net_pen_info,
          infected_net_pen_info
        )
        # Sample susceptible net pens for newly infected ones
        is_newly_infected <- stats::rbinom(
          nrow(susceptible_net_pens),
          1,
          prob_between_net_pen_infection
        )
        susceptible_net_pens <- cbind(susceptible_net_pens, is_newly_infected)
        newly_infected_ids <- susceptible_net_pens |>
          dplyr::filter(is_newly_infected == 1) |>
          dplyr::pull(net_pen_id)
        newly_infected_net_pens <-
          c(newly_infected_net_pens, newly_infected_ids)
      }

    # If any net pens have been newly infected
    if (length(newly_infected_net_pens) > 0) {
      # TODO: Should we have more than one animal become infected? How many?
      # Also entering in latent state by default because between net pen contact
      # is very distant
      n_infected_animals_by_stage = data.frame(
        # TODO: Modify number of latently infected fish to be a variable input parameter
        n_latent = rep(100, length(newly_infected_net_pens)),
        n_subclinical = rep(0, length(newly_infected_net_pens)),
        n_clinical = rep(0, length(newly_infected_net_pens))
      )
      # Add newly infected net pens to infected net pen info
      infected_net_pen_info <- he_add_infected_net_pen(
        infected_net_pen_info,
        simulation_env$net_pen_info,
        newly_infected_net_pens,
        n_infected_animals_by_stage,
        infection_origin = "between-net pen",
        simulation_day
      )
      # Add new infection stage durations for newly infected net pens
      infection_stage_distributions <-
        species_info[c(
          "latent_dur_freq",
          "subclinical_dur_freq",
          "clinical_dur_freq"
        )]
      for (i in 1:nrow(n_infected_animals_by_stage)) {
        simulation_env$infection_stage_duration_matrices$latent_duration <-
          he_add_infection_stage_duration(
            simulation_env$infection_stage_duration_matrices$latent_duration,
            species_info$latent_dur_freq[[1]],
            n_animals_to_distribute = n_infected_animals_by_stage[i, ]$n_latent
          )
        simulation_env$infection_stage_duration_matrices$subclinical_duration <-
          he_add_infection_stage_duration(
            simulation_env$infection_stage_duration_matrices$subclinical_duration,
            species_info$subclinical_dur_freq[[1]],
            n_animals_to_distribute = n_infected_animals_by_stage[
              i,
            ]$n_subclinical
          )
        simulation_env$infection_stage_duration_matrices$clinical_duration <-
          he_add_infection_stage_duration(
            simulation_env$infection_stage_duration_matrices$clinical_duration,
            species_info$clinical_dur_freq[[1]],
            n_animals_to_distribute = n_infected_animals_by_stage[
              i,
            ]$n_clinical
          )
      }
    }

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

    # Add and remove fish from different infection stages according to new
    # infections and the number of fish which have reached their duration in
    # their current infection stage
    infection_stage_counts <-
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
      he_update_infection_stage_counts(
        infection_stage_counts,
        simulation_env$infection_stage_duration_matrices,
        n_newly_infected,
        simulation_env$clinically_infected_prop
      )

    # Update infection stage duration matrices for animals entering a new stage
    n_latent_out <- simulation_env$infection_stage_duration_matrices$latent_duration[,
      1,
      drop = FALSE
    ]
    subclinical_clinical_split <-
      he_calculate_subclinical_clinical_infection_split(
        n_transitioning = n_latent_out,
        clinically_infected_prop = simulation_env$clinically_infected_prop
      )

    n_animals_transitioning_by_stage <-
      data.frame(
        n_latent_in = n_newly_infected,
        n_subclinical_in = subclinical_clinical_split[, 1],
        n_clinical_in = subclinical_clinical_split[, 2]
      )

    infection_stage_distributions <-
      species_info[c(
        "latent_dur_freq",
        "subclinical_dur_freq",
        "clinical_dur_freq"
      )]

    simulation_env$infection_stage_duration_matrices <-
      he_update_infection_stage_duration_matrix(
        simulation_env$infection_stage_duration_matrices,
        infection_stage_distribution = infection_stage_distributions,
        n_animals_to_distribute = n_animals_transitioning_by_stage
      )

    # Generate output for a day
    he_write_infected_net_pen_output(
      infected_net_pen_info,
      simulation_env$infected_net_pen_output_file_name,
      simulation_env$output_dir
    )

    return(infected_net_pen_info)
  } else {
    stop("No remaining infected net pens. Simulation should have terminated.")
  }
}
