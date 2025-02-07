#' Initialize model variables
#'
#' @param environment environment in which to store model level variables
#' @param farm_info_filepath filepath to farm info file
#' @param species_info_filepath filepath to species info file
#' @param connectivity_matrix_filepath filepath to connectivity matrix file
#' @param output_filepath filepath to the directory in which to write output files
#' @param ignore_status logical value indicating whether to ignore infection
#'    status values provided in input files or not
#'
#' @return NA
#' @export
#'
# References createHEvars in HEinitialize.R in hydroepix-model
he_initialize_model_env <-
  function(environment,
           farm_info_filepath,
           species_info_filepath,
           connectivity_matrix_filepath,
           output_filepath = "/output",
           ignore_status = TRUE) {
    # Read in farm info data frame to store and manage farm-related variables
    environment$farm_info <-
      he_read_farm_info_file(farm_info_filepath)

    # Read in species info data frame to store and manage species-related variables
    environment$species_info <-
      he_read_species_info_file(species_info_filepath)

    environment$connectivity_matrix <-
      he_read_connectivity_matrix_file(connectivity_matrix_filepath)

    # Initialize other internal simulation variables
    # TODO: Confirm if and where these are used - these can likely at least
    # be pared down for the basic model with only infection transmission
    # functionality
    he_initialize_internal_model_vars(environment)

    # Initialize additional variables in farm_info table and pull in relevant
    # species info
    environment$farm_info <-
      he_initialize_farm_info(environment$farm_info, environment$species_info)

    # Store number of farms as a separate variable due to frequent referencing
    # throughout the model
    environment$num_farms <- length(environment$farm_info$farm_id)

    # Set up output variables and corresponding output files
    # TODO: Review to see how much of this initialization is truly necessary
    he_initialize_inf_netpen_output(environment, output_filepath)
    he_initialize_result_summary_output(environment, output_filepath)
  }
