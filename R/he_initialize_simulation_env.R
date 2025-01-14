#' Initialize simulation environment
#'
#' @param environment simulation environment
#' @param farm_info_filepath filepath to farm info file
#' @param species_info_filepath filepath to species info file
#' @param dist_mat_filepath filepath to distance matrix file
#'
#' @return NA
#' @export
#'
he_initialize_simulation_env <-
  function(environment,
           farm_info_filepath,
           species_info_filepath,
           dist_mat_filepath,
           output_filepath = "/output",
           detailed_survey_output = FALSE,
           detailed_depop_output = FALSE,
           detailed_preemptive_depop_output = FALSE) {
    # Define farm object within environment to manage farm-related variables
    environment$farm_info <-
      he_read_farm_info_file(farm_info_filepath)

    # Define species object within environment to manage species-related variables
    environment$species_info <-
      he_read_species_info_file(species_info_filepath)

    # Define distance matrix within environment
    environment$dist_mat <-
      he_read_dist_mat_file(dist_mat_filepath)

    # Initialize other internal simulation variables
    he_initialize_internal_simulation_vars(environment)

    # Add new simulation-relevant variables from farm info file
    # Determine number of farms?
    environment$num_farms <- length(environment$farm_info$species)
    # TODO: Confirm "sus" does stand for susceptible?
    environment$farm_info$susceptible <- rep(NA, environment$num_farms)
    environment$farm_info$k <- rep(NA, environment$num_farms)
    environment$farm_info$depop_eligible <- rep(NA, environment$num_farms)

    # TODO: Check that this isn't a logical argument instead of a string?
    # Check depop-eligibility by species
    if (!identical(environment$species_to_depop, "all")) {
      environment$farm_info$depop_eligible <-
        environment$farm_info$species %in% environment$species_to_depop
    }

    # Set up output variables and corresponding output files
    he_initialize_inf_netpen_output(environment, output_filepath)
    he_initialize_result_summary_output(environment, output_filepath)

    if (detailed_survey_output) {
      he_initialize_survey_output(environment, output_filepath)
    }
    if (detailed_depop_output) {
      he_initialize_depop_output(environment, output_filepath)
    }
    if (detailed_preemptive_depop_output) {
      he_initialize_preemptive_depop_output(environment, output_filepath)
    }
  }
