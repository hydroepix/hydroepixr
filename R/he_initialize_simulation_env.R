he_initialize_simulation_env <-
  function(environment,
           farm_info_filepath,
           types_filepath,
           dist_mat_filepath) {
    # Define farm object within environment to manage farm-related variables
    simulation_env$farm_info <-
      he_read_farm_info_file("inst/testdata/farm_file_bay_x.csv")

    # Define species object within environment to manage species-related variables
    environment$species_info <-
      he_read_types_file("inst/testdata/types_file_bay_x.csv")

    # Define distance matrix within environment
    environment$dist_mat <-
      he_read_dist_mat_file("inst/testdata/dist_mat_bay_x.csv")


    # TODO: Function to initialize these variables?
    #he_expand_farm_info? he_initialize_simulation_farm_info?
    # Add new simulation-relevant variables from farm info file
    # Determine number of farms?
    environment$num_farms <- length(farm_info$farm_type)
    # TODO: Confirm "sus" does stand for susceptible?
    environment$farm_info$susceptible <- rep(NA, g_max_farms)
    environment$farm_info$k <- rep(NA, g_max_farms)
    environment$farm_info$cull_eligible <- rep(NA, g_max_farms)

    # TODO: Check that this isn't a logical argument instead of a string?
    # Check cull-eligibility by species
    if (!identical(environment$species_to_cull, "all")) {
      environment$farm_info$cull_eligible <-
        environment$farm_info$farm_type %in% environment$species_to_cull
    }

    # TODO: Check which functions to use? Maybe better elsewhere
    # eval.parent(expression(
    #   RFtrans <- switch(RFstoch + 1,
    #                     function(z,nn,pp){ nn*pp }, # if False
    #                     rbinom                      # if True
    #   )
    # ))
    #
    # eval.parent(expression(
    #   Tinfs <- switch(Tstoch + 1,
    #                   function(contactHerds,dProbInfection,Sus) {
    #                     rbinom(length(contactHerds),1,dProbInfection)
    #                   },
    #                   function(contactHerds,dProbInfection,Sus) {
    #                     rbinom(length(contactHerds),aHerd$Sus[contactHerds],
    #                            1 - (1 - dProbInfection) ^ (1 / Sus[contactHerds]))
    #                   })
    # ))

    # TODO: Move output generation into a different function?

    # Set up infected cage matrix and output file
    environment$all_inf_cages <- matrix(numeric(0), ncol = 10)
    environment$infected_output_file_name <-
      paste(run_id, "all_inf_cages.txt", sep = "-")
    write.table(all_inf_cages, infected_output_file_name, sep = " ")

    # Set up result matrix?? and output file
    environment$result_summary_output <-
      matrix(numeric(0), ncol = 10)
    # TODO: Allow custom naming of this file?
    environment$result_summary_file_name <- paste(run_id, "isa.txt")
    write.table(environment$result_summary_output,
                environment$result_summary_file_name,
                sep = " ")

    if (environment$detailed) {
      environment$surveyed_matrix_output <- matrix(numeric(0), ncol = 3)
      environment$depopulated_matrix_output <-
        matrix(numeric(0), ncol = 3)
      # TODO: Figure out what this is
      environment$preempted_matrix_output <-
        matrix(numeric(0), ncol = 3)

      surveyed_output_file_name <-
        paste(run_id, "surveyed_farms.txt", sep = "-")
      depopulated_output_file_name <-
        paste(run_id, "depopulated_farms.txt", sep = "-")
      preempted_output_file_name <-
        paste(run_id, "preempted_farms.txt", sep = "-")

      write.table(
        surveyed_matrix_output,
        surveyed_output_file_name,
        col.names = FALSE,
        row.names = FALSE
      )
      write.table(
        depopulated_matrix_output,
        depopulated_output_file_name,
        col.names = FALSE,
        row.names = FALSE
      )
      write.table(
        preempted_matrix_output,
        preempted_output_file_name,
        col.names = FALSE,
        row.names = FALSE
      )
    }
  }
