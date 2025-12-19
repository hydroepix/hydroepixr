#' Read Species Info File
#'
#' @param filepath to species info file
#'
#' @return data frame of species info
#' @export
#' @importFrom utils read.table

he_read_species_info_file <- function(filepath) {
  species_info <- utils::read.table(
    filepath,
    sep = ",",
    as.is = TRUE,
    header = TRUE
  )
  species_info_cols <- names(species_info)
  expected_cols <- list(
    "species_id",
    "species_name",
    "latent_dur_freq",
    "subclinical_dur_freq",
    "clinical_dur_freq",
    "within_net_pen_transmission_min",
    "within_net_pen_transmission_mode",
    "within_net_pen_transmission_max",
    "rel_susceptibility"
  )
  mismatched_cols <- setdiff(species_info_cols, expected_cols)
  if (length(mismatched_cols > 0)) {
    stop(
      "Unexpected column headers. Expected headers are: ",
      paste(expected_cols, collapse = ", "),
      "\nHeaders in the provided file that do not match are: ",
      paste(mismatched_cols, collapse = ", ")
    )
  }
  # Parsing R-code style vectors in input file
  # Note that this assumes a single row of species info data
  species_info <- he_parse_disease_stage_distributions(species_info)
  species_info
}
