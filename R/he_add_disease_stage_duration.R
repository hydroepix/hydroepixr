#' Add new row to disease stage duration matrix
#'
#' @param duration_matrix matrix of days animals will spend in the specified
#'    disease stage
#' @param disease_stage_distribution probability distribution of an animal
#'    spending a given number of days in the specified disease stage
#' @param n_animals_to_distribute number of animals to distribute into time
#'    spent in the specified disease stage
#'
#' @export
#'
he_add_disease_stage_duration <-
  function(duration_matrix,
           disease_stage_distribution,
           n_animals_to_distribute) {
    distributed_animals <- he_rpoly2(n_animals_to_distribute,
                                     disease_stage_distribution)
    rbind(duration_matrix, distributed_animals, deparse.level = 0)
  }
