# Generates rows indicating animals entering a specified stage
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
  t(apply(cbind(new_inf_rows[, col_index],
                species_info$col_name[new_inf_rows[, 17], , drop = FALSE])))
}
