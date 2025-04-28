#' Initialize model variables
#'
#' @param model_env environment in which to store model level variables
#' @param farm_info_filepath filepath to farm info file
#' @param species_info_filepath filepath to species info file
#' @param connectivity_matrix_filepath filepath to connectivity matrix file
#'
#' @return NA
#' @export
#'
# References createHEvars in HEinitialize.R in hydroepix-model
he_initialize_model_env <-
  function(model_env,
           farm_info_filepath,
           species_info_filepath,
           connectivity_matrix_filepath) {
    # Read in files data
    model_env$farm_info <-
      he_read_farm_info_file(farm_info_filepath)

    model_env$species_info <-
      he_read_species_info_file(species_info_filepath)

    model_env$connectivity_matrix <-
      he_read_connectivity_matrix_file(connectivity_matrix_filepath)

    # Initialize variables for internal tracking
    model_env$index_netpens <- NA
    model_env$infected_netpens <- NULL

    # Initialize additional variables in farm_info table and pull in relevant
    # species info
    model_env$farm_info <-
      he_initialize_farm_info(model_env$farm_info, model_env$species_info)

    # Store number of farms as a separate variable due to frequent referencing
    # throughout the model
    model_env$num_netpens <- length(model_env$farm_info$netpen_id)
  }
