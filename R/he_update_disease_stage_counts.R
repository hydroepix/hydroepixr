#' Update counts of animals transitioning from one disease stage into another
#'
#' @param disease_stage_counts data frame containing the counts of animals in
#'    each disease stage, where each disease stage is a column and each row is
#'    a net pen
#' @param disease_stage_duration_matrices list of matrices containing the
#'    disease stage duration for each net pen
#' @param n_newly_infected the number of newly infected animals for each
#'    net pen
#' @param clinically_infected_prop the proportion of animals which will enter
#'    the clinical stage upon infection, which is the same as 1 minus the
#'    proportion of animals which will enter the subclinical stage upon
#'    infection
#'
#' @return modified disease stage count columns
#' @export

he_update_disease_stage_counts <- function(
  disease_stage_counts,
  disease_stage_duration_matrices,
  n_newly_infected,
  clinically_infected_prop
) {
  # This function assumes at least four disease stages because of the call to
  # he_calculate_net_change_in_disease_stage_count
  if (ncol(disease_stage_counts) < 4) {
    stop(
      "At least four disease stage counts are required in order to
         calculate transition between a beginning stage, an intermediate stage,
         and two final stages of disease. Please include at least four disease
         stage count columns."
    )
  }

  # The number of infected net pens (rows) should be consistent across disease
  # stage duration matrices
  n_rows_disease_stage_duration_matrices <-
    sapply(disease_stage_duration_matrices, nrow)

  if (length(unique(n_rows_disease_stage_duration_matrices)) > 1) {
    stop(
      "Inconsistent number of rows in disease stage duration matrices.
         Different matrices have numbers of rows as follows: ",
      paste(n_rows_disease_stage_duration_matrices, collapse = ", ")
    )
  }

  # The number of net pens should be consistent across provided arguments:
  # Length of newly infected
  # Number of rows in disease stage counts
  # Number of rows in duration matrices
  if (nrow(disease_stage_counts) != length(n_newly_infected)) {
    stop(
      "Mismatched number of infected net pens (rows) between disease stage
         counts and number of newly infected animals. The number of newly
         infected animal values should be equal to the number of rows of
         disease stage counts.\n
         Number of rows in disease stage counts: ",
      nrow(disease_stage_counts),
      "\n
         Number of newly infected animal values: ",
      length(n_newly_infected)
    )
  }
  if (
    !all(nrow(disease_stage_counts) == n_rows_disease_stage_duration_matrices)
  ) {
    stop(
      "Mismatched number of infected net pens (rows) between disease stage
         counts and disease stage duration matrices. All duration matrices and
         the disease stage counts should all have the same number of rows.\n
         Number of rows in each disease stage duration matrix: ",
      paste(n_rows_disease_stage_duration_matrices, collapse = ", "),
      "\n
         Number of rows in disease stage counts: ",
      nrow(disease_stage_counts)
    )
  }

  # There should be exactly three more disease stage counts than there are
  # disease stage matrices, which accounts for the starting stage (susceptible)
  # and the ending stages (recovered and dead)
  if (
    ncol(disease_stage_counts) != length(disease_stage_duration_matrices) + 3
  ) {
    stop(
      "Mismatched number of disease stage counts and disease stage duration
         matrices. The number of disease stage duration matrices should be three
         less than the number of disease stage counts. \n
         Number of disease stage duration matrices: ",
      length(disease_stage_duration_matrices),
      "\n
         Number of disease stage counts: ",
      ncol(disease_stage_counts)
    )
  }

  net_change <-
    he_calculate_net_change_in_disease_stage_count(
      disease_stage_duration_matrices,
      n_newly_infected,
      clinically_infected_prop
    )

  updated_disease_stage_counts <- disease_stage_counts + net_change
  updated_disease_stage_counts
}
