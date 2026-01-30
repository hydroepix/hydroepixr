#' Update disease duration stage matrices with newly distributed animals
#'
#' @param disease_stage_distribution data frame of species information
#' @param disease_stage_matrix matrix representing a disease stage, recording
#'    the number of animals that will spend a given number of days in that
#'    disease stage
#' @param n_animals_to_distribute number of animals which are newly entering
#'    the given disease stage
#'
#' @return updated disease stage duration matrix with newly distributed animals
#'
#'
he_update_disease_stage_duration_matrix <-
  function(
    disease_stage_matrix,
    disease_stage_distribution,
    n_animals_to_distribute
  ) {
    n_disease_stages <- ncol(n_animals_to_distribute)
    disease_stage_matrix_names <- names(disease_stage_matrix)
    for (i in 1:n_disease_stages) {
      # Generate a new set of durations for incoming animals to spend in this
      # disease stage before transitioning
      n_animals_to_distribute_in_stage <- n_animals_to_distribute[, i]
      disease_stage_distribution_for_stage <- disease_stage_distribution[[i]][[
        1
      ]]
      newly_distributed_animals_in_stage <- purrr::map(
        n_animals_to_distribute_in_stage,
        he_rpoly2,
        disease_stage_distribution_for_stage
      )
      # Convert to matrix to allow simple matrix addition
      newly_distributed_animals_in_stage <- matrix(
        unlist(newly_distributed_animals_in_stage),
        ncol = length(newly_distributed_animals_in_stage[[1]]),
        byrow = TRUE
      )
      # Update current disease matrix for the stage by a "sliding window"
      disease_stage_matrix_for_stage <- disease_stage_matrix[[i]]
      # First index, i.e. "today" is dropped, since these animals will transition
      # to the next stage
      # TODO: Have transitioned to the next stage?
      # Zero column is appended to the end to accommodate length of newly
      # distributed animals being added
      updated_disease_stage_matrix_for_stage <-
        cbind(disease_stage_matrix_for_stage[, -1, drop = FALSE], 0)
      # New durations are then added columnwise
      updated_disease_stage_matrix_for_stage <-
        updated_disease_stage_matrix_for_stage +
        newly_distributed_animals_in_stage
      # Assign updated version to list of disease_stage_matrix
      disease_stage_matrix[[i]] <- updated_disease_stage_matrix_for_stage
    }
    names(disease_stage_matrix) <- disease_stage_matrix_names
    return(disease_stage_matrix)
  }
