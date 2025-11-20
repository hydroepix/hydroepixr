#' Calculate number of newly subclinically and clinically infected animals
#'
#' @param n_transitioning number of animals transitioning out of the latent stage
#' @param clinically_infected_prop proportion of animals which should become
#'    clinically infected
#'
#' @returns matrix with two columns, the first representing the number of newly
#'    subclinically infected animals and the second representing the number of
#'    newly clinically infected animals
#' @export
#'
he_calculate_subclinical_clinical_infection_split <-
  function(n_transitioning,
           clinically_infected_prop) {
    n_newly_clinical <- as.matrix(ceiling(n_transitioning * clinically_infected_prop))
    n_newly_subclinical <- as.matrix(n_transitioning - n_newly_clinical)
    cbind(n_newly_subclinical, n_newly_clinical, deparse.level = 0)
  }
