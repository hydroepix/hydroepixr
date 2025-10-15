#' Calculate net change in number of animals in each disease stage
#'
#' @param disease_stage_duration_matrices list of matrices containing the
#'    disease stage duration for each netpen
#' @param n_newly_infected the number of newly infected animals for each
#'    netpen
#' @param clinically_infected_prop the proportion of animals which will enter
#'    the clinical stage upon infection, which is the same as 1 minus the
#'    proportion of animals which will enter the subclinical stage upon
#'    infection
#'
#' @return net change in the number of animals in each disease stage
#' @export
#'
he_calculate_net_change_in_disease_stage_count <-
  function(disease_stage_duration_matrices,
           n_newly_infected,
           clinically_infected_prop) {
    # First column of the matrix represents number of fish that will transition
    # today
    changes_in_disease_stage_duration <-
      matrix(
        unlist(lapply(disease_stage_duration_matrices, \(x) x[, 1])),
        ncol = length(disease_stage_duration_matrices),
        byrow = FALSE
      )
    # Upon infection, animals either enter the latent stage or bypass it entirely
    # Upon exiting the latent stage, animals will become either subclinically
    # or clinically infected, depending on the "clinically_infected_prop"
    # For latent animals, determine how many will become clinical and how many
    # will become subclinical - this is the first element in the vector of
    # changes in disease stage duration calculated from the disease stage
    # duration matrices
    subclinical_clinical_split <-
      he_calculate_subclinical_clinical_infection_split(
        changes_in_disease_stage_duration[1],
        clinically_infected_prop)

    animals_in_by_stage <-
      cbind(0, n_newly_infected, subclinical_clinical_split,
            deparse.level = 0)
    animals_out_by_stage <-
      cbind(n_newly_infected, subclinical_clinical_split, 0,
            deparse.level = 0)
    animals_in_by_stage - animals_out_by_stage
  }
