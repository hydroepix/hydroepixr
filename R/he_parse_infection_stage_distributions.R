#' Parse infection stage distributions
#'
#' @param species_info_row a row of species info data, including headers
#'
#' @return species info data frame with infection distribution column inputs parsed
#'    from literal text to evaluated code expressions
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr contains
he_parse_infection_stage_distributions <- function(species_info_row) {
  species_info_row <- dplyr::mutate(
    species_info_row,
    dplyr::across(dplyr::contains("dur_freq"), \(x) list(eval(parse(text = x))))
  )
  return(species_info_row)
}
