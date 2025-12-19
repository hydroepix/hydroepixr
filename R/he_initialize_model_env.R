#' Initialize model variables
#'
#' @param model_env environment in which to store model level variables
#' @param net_pen_info_filepath filepath to net pen info file
#' @param species_info_filepath filepath to species info file
#' @param connectivity_matrix_filepath filepath to connectivity matrix file
#'
#' @return NA
#' @export
#'
# References createHEvars in HEinitialize.R in hydroepix-model
he_initialize_model_env <-
  function(
    model_env,
    net_pen_info_filepath,
    species_info_filepath,
    connectivity_matrix_filepath = NULL
  ) {
    # Read in files data
    model_env$net_pen_info <-
      he_read_net_pen_info_file(net_pen_info_filepath)

    model_env$species_info <-
      he_read_species_info_file(species_info_filepath)

    if (!is.null(connectivity_matrix_filepath)) {
      model_env$connectivity_matrix <-
        he_read_connectivity_matrix_file(connectivity_matrix_filepath)
    }

    # Initialize variables for internal tracking
    model_env$index_net_pens <- NA
    model_env$infected_net_pens <- NULL

    # Initialize additional variables in net_pen_info table and pull in relevant
    # species info
    model_env$net_pen_info <-
      he_initialize_net_pen_info(model_env$net_pen_info, model_env$species_info)

    # Store number of net_pens as a separate variable due to frequent referencing
    # throughout the model
    model_env$n_net_pens <- length(model_env$net_pen_info$net_pen_id)
  }
