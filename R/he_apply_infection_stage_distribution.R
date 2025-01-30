#' Applies distribution to determine infection stage of newly infected animals
#'
#' @param new_inf_rows matrix with rows representing newly infected netpens
#' @param species_info matrix of information on species
#' @param stage infection stage the newly infected animals are entering
#'
#' @return ?
#' @export
#'
he_apply_infection_stage_distribution <- function(new_inf_rows, species_info, stage) {
  if (stage == "latent") {
    col_index <- 2
    col_name <- "latent_dur_freq"
  } else if (stage == "subclinical") {
    col_index <- 3
    col_name <- "subclinical_dur_freq"
  } else if (stage == "clinical") {
    col_index <- 4
    col_name <- "clinical_dur_freq"
  } else {
    stop("Invalid infection stage entered.")
  }
  # TODO: Can this be reworked or extracted into a separate function?
  # and why is the transpose necessary?
  t(apply(cbind(new_inf_rows[, col_index],
                species_info$col_name[new_inf_rows[, 17], , drop = FALSE])))
}
