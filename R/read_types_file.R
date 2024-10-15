#' Read Types File
#'
#' @param filepath to types file
#'
#' @return data frame of type info from types file
#' @export
#' @importFrom utils read.table
#'
#' @examples
#' test_types_file <- c(
#'   1,
#'   "Fish1",
#'   "c(0.000001,0.000001,0.0001,0.01,0.1,0.25,0.339898,0.3)",
#'   "c(0.249995,0.50,0.25,0.000001,0.000001,0.000001,0.000001,0.000001)",
#'   "c(0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125)",
#'   "rpert(n,0.14,0.4,0.8)",
#'   1
#' )
#' tf <- tempfile()
#' writeLines(test_types_file, tf)
#' read_farm_info_file(tf)
read_types_file <- function(filepath) {
  type_info <- as.list(utils::read.table(
    filepath,
    sep = ",",
    as.is = TRUE,
    header = TRUE
  ))
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
  for (i in 3:length(type_info)) {
    type_info[[i]] <- eval(parse(text = type_info[[i]]))
  }
  type_info
}

#test <- read_types_file("data/types_file_bay_x.csv")
