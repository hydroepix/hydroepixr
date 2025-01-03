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
           dist_mat_filepath) {
    # Define farm object within environment to manage farm-related variables
    simulation_env$farm_info <-
      he_read_farm_info_file("inst/testdata/farm_file_bay_x.csv")

    # Define species object within environment to manage species-related variables
    environment$species_info <-
      he_read_species_info_file("inst/testdata/types_file_bay_x.csv")

    # Define distance matrix within environment
    environment$dist_mat <-
      he_read_dist_mat_file("inst/testdata/dist_mat_bay_x.csv")

    # Initialize other internal simulation variables
    he_initialize_internal_simulation_variables()

    # Add new simulation-relevant variables from farm info file
    # Determine number of farms?
    environment$num_farms <- length(farm_info$species)
    # TODO: Confirm "sus" does stand for susceptible?
    environment$farm_info$susceptible <- rep(NA, g_max_farms)
    environment$farm_info$k <- rep(NA, g_max_farms)
    environment$farm_info$depop_eligible <- rep(NA, g_max_farms)

    # TODO: Check that this isn't a logical argument instead of a string?
    # Check depop-eligibility by species
    if (!identical(environment$species_to_depop, "all")) {
      environment$farm_info$depop_eligible <-
        environment$farm_info$species %in% environment$species_to_depop
    }

    # Set up output variables and corresponding output files
    he_initialize_infected_netpen_output()
    he_initialize_result_summary_output()

    if (environment$detailed) {
      he_initialize_survey_output()
      he_initialize_depopulation_output()
      # TODO: Figure out what this is
      he_initialize_preemptive_depop_output()
    }
  }
