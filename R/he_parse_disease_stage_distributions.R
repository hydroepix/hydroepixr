he_parse_disease_stage_distributions <- function(species_info_row) {
  species_info_row <- dplyr::mutate(species_info_row,
                                    dplyr::across(dplyr::contains("dur_freq"),
                                                  \(x) list(eval(parse(text = x)))))
}
