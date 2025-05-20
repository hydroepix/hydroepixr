#' Calculate net change in number of animals in each disease stage
#'
#' @param disease_stage_duration_matrices list of matrices containing the
#'    disease stage duration for each netpen
#' @param n_newly_infected he number of newly infected animals for each
#'    netpen
#'
#' @return net change in the number of animals in each disease stage
#' @export
#'
he_calculate_net_change_in_disease_stage_count <-
  function(disease_stage_duration_matrices,
           n_newly_infected) {
    # First column of the matrix represents number of fish that will transition
    # today
    changes_in_disease_stage_duration <-
      matrix(
        unlist(lapply(disease_stage_duration_matrices, \(x) x[, 1])),
        ncol = length(disease_stage_duration_matrices),
        byrow = FALSE
      )
    animals_in_by_stage <-
      cbind(0, n_newly_infected, changes_in_disease_stage_duration,
            deparse.level = 0)
    animals_out_by_stage <-
      cbind(n_newly_infected, changes_in_disease_stage_duration, 0,
            deparse.level = 0)
    animals_in_by_stage - animals_out_by_stage
  }
