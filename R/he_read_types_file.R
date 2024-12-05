#' Read Types File
#'
#' @param filepath to types file
#'
#' @return data frame of type info from types file
#' @export
#' @importFrom utils read.table

he_read_types_file <- function(filepath) {
  type_info <- as.list(utils::read.table(
    filepath,
    sep = ",",
    as.is = TRUE,
    header = TRUE
  ))
  type_info_columns <- names(type_info)
  expected_columns <- list(
    "farm_type_id",
    "farm_type_name",
    "lat_dur_freq",
    "sub_dur_freq",
    "cli_dur_freq",
    "within_pen_transmission",
    "rel_susceptibility"
  )
  mismatched_columns <- setdiff(type_info_columns, expected_columns)
  if (length(mismatched_columns > 0)) {
    stop(
      "Unexpected column headers. Expected headers are: ",
      paste(expected_columns, collapse = ", "),
      "\nHeaders in the provided file that do not match are: ",
      paste(mismatched_columns, collapse = ", ")
    )
  }
  # TODO: Figure out how this might need to be modified if tornado plot option
  # is selected?
  # Code should likely go elsewhere
  ## modify one column of herdtypes table if tornado plot option is selected
  # if (!is.null(tornCol)) {
  #
  #   if (is.numeric(herdtypes[[tornCol]])) {
  #     herdtypes[[tornCol]] <<- 0.9 * herdtypes[[tornCol]]
  #   } else {
  #     herdtypes[[tornCol]] <<- paste(as.character(tornMult),
  #                                    "*",herdtypes[[tornCol]])
  #   }
  # }
  #
  # TODO: Can we modify the inputs so that we don't have to store these as
  # expressions? Can we have functions to manage these distributions?
  ## Parsing verbatim code in input file
  # for (i in 3:length(type_info)) {
  #   type_info[[i]] <- eval(parse(text = type_info[[i]]))
  # }
  type_info
}
