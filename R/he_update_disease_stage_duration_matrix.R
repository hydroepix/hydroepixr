#' Update disease duration stage matrices by with newly distributed animals
#'
#' @param disease_stage_distribution data frame of species information
#' @param disease_stage_matrix matrix representing a disease stage, recording
#'    the number of animals that will spend a given number of days in that
#'    disease stage
#' @param num_animals_to_distribute number of animals which are newly entering
#'    the given disease stage
#'
#' @export
#'
he_update_disease_stage_duration_matrix <-
  function(disease_stage_matrix,
           disease_stage_distribution,
           num_animals_to_distribute) {
  # Generate a new set of durations for incoming animals to spend in this
  # disease stage before transitioning
  newly_distributed_animals <-
    t(he_rpoly2(num_animals_to_distribute,
                disease_stage_distribution))

  # Slide window along durations
  # First index, i.e. "today" is dropped, since these animals will transition
  # to the next stage
  # Zero column is appended to the end to accommodate length of newly generated
  # duration set
  # New durations are then added columnwise
  disease_stage_matrix <-
    cbind(disease_stage_matrix[,-1, drop = FALSE], 0) + newly_distributed_animals
}
