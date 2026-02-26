#' Update counts of animals transitioning from one infection stage into another
#'
#' @param infection_stage_counts data frame containing the counts of animals in
#'    each infection stage, where each infection stage is a column and each row is
#'    a net pen
#' @param infection_stage_duration_matrices list of matrices containing the
#'    infection stage duration for each net pen
#' @param n_newly_infected the number of newly infected animals for each
#'    net pen
#' @param clinically_infected_prop the proportion of animals which will enter
#'    the clinical stage upon infection, which is the same as 1 minus the
#'    proportion of animals which will enter the subclinical stage upon
#'    infection
#'
#' @return modified infection stage count columns

he_update_infection_stage_counts <- function(
  infection_stage_counts,
  infection_stage_duration_matrices,
  n_newly_infected,
  clinically_infected_prop
) {
  # This function assumes at least four infection stages because of the call to
  # he_calculate_net_change_in_infection_stage_count
  if (ncol(infection_stage_counts) < 4) {
    stop(
      "At least four infection stage counts are required in order to
         calculate transition between a beginning stage, an intermediate stage,
         and two final stages of infection. Please include at least four infection
         stage count columns."
    )
  }

  # The number of infected net pens (rows) should be consistent across infection
  # stage duration matrices
  n_rows_infection_stage_duration_matrices <-
    sapply(infection_stage_duration_matrices, nrow)

  if (length(unique(n_rows_infection_stage_duration_matrices)) > 1) {
    stop(
      "Inconsistent number of rows in infection stage duration matrices.
         Different matrices have numbers of rows as follows: ",
      paste(n_rows_infection_stage_duration_matrices, collapse = ", ")
    )
  }

  # The number of net pens should be consistent across provided arguments:
  # Length of newly infected
  # Number of rows in infection stage counts
  # Number of rows in duration matrices
  if (nrow(infection_stage_counts) != length(n_newly_infected)) {
    stop(
      "Mismatched number of infected net pens (rows) between infection stage
         counts and number of newly infected animals. The number of newly
         infected animal values should be equal to the number of rows of
         infection stage counts.\n
         Number of rows in infection stage counts: ",
      nrow(infection_stage_counts),
      "\n
         Number of newly infected animal values: ",
      length(n_newly_infected)
    )
  }
  if (
    !all(
      nrow(infection_stage_counts) == n_rows_infection_stage_duration_matrices
    )
  ) {
    stop(
      "Mismatched number of infected net pens (rows) between infection stage
         counts and infection stage duration matrices. All duration matrices and
         the infection stage counts should all have the same number of rows.\n
         Number of rows in each infection stage duration matrix: ",
      paste(n_rows_infection_stage_duration_matrices, collapse = ", "),
      "\n
         Number of rows in infection stage counts: ",
      nrow(infection_stage_counts)
    )
  }

  # There should be exactly three more infection stage counts than there are
  # infection stage matrices, which accounts for the starting stage (susceptible)
  # and the ending stages (recovered and dead)
  if (
    ncol(infection_stage_counts) !=
      length(infection_stage_duration_matrices) + 3
  ) {
    stop(
      "Mismatched number of infection stage counts and infection stage duration
         matrices. The number of infection stage duration matrices should be three
         less than the number of infection stage counts. \n
         Number of infection stage duration matrices: ",
      length(infection_stage_duration_matrices),
      "\n
         Number of infection stage counts: ",
      ncol(infection_stage_counts)
    )
  }

  net_change <-
    he_calculate_net_change_in_infection_stage_count(
      infection_stage_duration_matrices,
      n_newly_infected,
      clinically_infected_prop
    )

  updated_infection_stage_counts <- infection_stage_counts + net_change
  return(updated_infection_stage_counts)
}
