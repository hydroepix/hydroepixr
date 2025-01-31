#' Read Species Info File
#'
#' @param filepath to species info file
#'
#' @return data frame of species info
#' @export
#' @importFrom utils read.table

he_read_species_info_file <- function(filepath) {
  species_info <- as.list(utils::read.table(
    filepath,
    sep = ",",
    as.is = TRUE,
    header = TRUE
  ))
  species_info_columns <- names(species_info)
  expected_columns <- list(
    "species_id",
    "species_name",
    "latent_dur_freq",
    "subclinical_dur_freq",
    "clinical_dur_freq",
    "within_netpen_transmission",
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
  #
  # TODO: Can we modify the inputs so that we don't have to store these as
  # expressions? Can we have functions to manage these distributions?
  ## Parsing verbatim code in input file
  # for (i in 3:length(type_info)) {
  #   type_info[[i]] <- eval(parse(text = type_info[[i]]))
  # }
  species_info
}
