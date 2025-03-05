#' Update counts of animals transitioning from one disease stage into another
#'
#' @param disease_stage_counts data frame containing the counts of animals in
#'    each disease stage, where each disease stage is a column and each row is
#'    a netpen
#' @param disease_stage_duration_matrices list of matrices containing the
#'    disease stage duration for each netpen
#' @param num_newly_infected the number of newly infected animals for each
#'    netpen
#'
#' @return modified disease stage count columns
#' @export

he_update_disease_stage_counts <- function(disease_stage_counts,
                                           disease_stage_duration_matrices,
                                           num_newly_infected) {
  # There should be exactly two more disease stage counts than there are
  # disease stage matrices, which accounts for the starting stage (susceptible)
  # and the ending stage (immune, which includes recovered and dead)
  if (ncol(disease_stage_counts) ==
      length(disease_stage_duration_matrices) + 2) {
    # TODO: Other checks to make:
    # 1. At least 3 disease stage counts because we assume a starting,
    # intermediate, and ending  disease stage at minimum
    # 2. Length of newly infected should equal the number of rows in disease
    # stage counts and number of rows in duration matrices
    # (i.e. number of netpens matches up)

    # Store the net change for each disease stage, based on the number of newly
    # infected and/or the disease stage duration matrices
    net_change <-
      he_calculate_net_change_in_disease_stage_count(
        disease_stage_duration_matrices,
        num_newly_infected
      )
    disease_stage_counts + net_change
  } else {
    stop("Mismatched number of disease stage counts and disease stage duration
         matrices. The number of disease stage duration matrices should be two
         less than the number of disease stage counts. \n
         Number of disease stage duration matrices: ",
         length(disease_stage_duration_matrices), "\n
         Number of disease stage counts: ",
         ncol(disease_stage_counts) )
  }

}

