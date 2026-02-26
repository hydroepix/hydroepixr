#' Calculate net change in number of animals in each infection stage
#'
#' @param infection_stage_duration_matrices list of matrices containing the
#'    infection stage duration for each net pen
#' @param n_newly_infected the number of newly infected animals for each
#'    net pen
#' @param clinically_infected_prop the proportion of animals which will enter
#'    the clinical stage upon infection, which is the same as 1 minus the
#'    proportion of animals which will enter the subclinical stage upon
#'    infection
#'
#' @return net change in the number of animals in each infection stage
#'
he_calculate_net_change_in_infection_stage_count <-
  function(
    infection_stage_duration_matrices,
    n_newly_infected,
    clinically_infected_prop
  ) {
    # First column of the matrix represents number of fish that will transition
    # today
    changes_in_infection_stage_duration <-
      matrix(
        unlist(lapply(infection_stage_duration_matrices, \(x) x[, 1])),
        ncol = length(infection_stage_duration_matrices),
        byrow = FALSE
      )
    # Upon infection, animals either enter the latent stage or bypass it entirely
    # Upon exiting the latent stage, animals will become either subclinically
    # or clinically infected, depending on the "clinically_infected_prop"
    # For latent animals, determine how many will become clinical and how many
    # will become subclinical - this is the first element in the vector of
    # changes in infection stage duration calculated from the infection stage
    # duration matrices
    subclinical_clinical_split <-
      he_calculate_subclinical_clinical_infection_split(
        as.matrix(changes_in_infection_stage_duration[, 1]),
        clinically_infected_prop
      )

    # Necessary to force into column-wise matrix for cbind
    subclinical_clinical_split <- matrix(subclinical_clinical_split, ncol = 2)

    animals_in_by_stage <-
      cbind(
        # into susceptible  (i.e. none... at this time)
        0,
        # into latent (i.e. newly infected)
        n_newly_infected,
        # into subclinical and clinical (i.e. out of latent, split
        # into subclinical and clinical)
        subclinical_clinical_split,
        # into recovered (i.e. out of subclinical)
        changes_in_infection_stage_duration[, 2],
        # into dead (i.e. out of clinical)
        changes_in_infection_stage_duration[, 3],
        deparse.level = 0
      )
    animals_out_by_stage <-
      cbind(
        # out of susceptible (i.e. newly infected)
        n_newly_infected,
        # out of latent (i.e. split into subclinical and clinical)
        changes_in_infection_stage_duration[, 1],
        # out of subclinical (i.e. into recovered)
        changes_in_infection_stage_duration[, 2],
        # out of clinical (i.e. into dead)
        changes_in_infection_stage_duration[, 3],
        # out of recovered (i.e. none... at this time)
        0,
        # out of dead (i.e. none... ever)
        0,
        deparse.level = 0
      )
    return(animals_in_by_stage - animals_out_by_stage)
  }
