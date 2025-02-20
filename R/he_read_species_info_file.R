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
  species_info_columns <- names(species_info)
  expected_columns <- list(
    "species_id",
    "species_name",
    "latent_dur_freq",
    "subclinical_dur_freq",
    "clinical_dur_freq",
    "within_netpen_transmission_min",
    "within_netpen_transmission_mode",
    "within_netpen_transmission_max",
    "rel_susceptibility"
  )
  mismatched_columns <- setdiff(species_info_columns, expected_columns)
  if (length(mismatched_columns > 0)) {
    stop(
      "Unexpected column headers. Expected headers are: ",
      paste(expected_columns, collapse = ", "),
      "\nHeaders in the provided file that do not match are: ",
      paste(mismatched_columns, collapse = ", ")
    )
  }
  # Parsing R-code style vectors in input file
  # Note that this assumes a single row of species info data
  species_info <- he_parse_disease_stage_distributions(species_info)
  species_info
}
