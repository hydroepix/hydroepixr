he_define_simulation_control_params <-
  function(environment,
           n = 1,
           run_id = NULL,
           step_in_file = NULL,
           max_outbreak_length = 365,
           random_seed = -10,
           ignore_disease_status_input = TRUE,
           verbose = FALSE,
           summary_function = "HEsum") {
    environment$n <- n
    environment$run_id <- run_id
    environment$step_in_file <- step_in_file
    environment$max_outbreak_length <- 365
    environment$random_seed <- random_seed
    environment$ignore_disease_status_input <-
      ignore_disease_status_input
    environment$verbose <- verbose
    environment$summary_function <- "HEsum"
  }
