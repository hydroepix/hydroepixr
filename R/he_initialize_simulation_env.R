he_initialize_simulation_env <- function(environment, num_farms, farm_info) {
  # gDaysUntilBaseline - determine the day the first detection will happen
  environment$days_until_baseline_mort <- 0

  # TotNumQueue - output variable to calculate the number of "herds" queueing
  # for surveillance per day
  # does not affect anything (TH) ?
  environment$total_num_queued <- 0

  # Initialize waiting periods and transmission probabilities
  farm_info$tagged_duration <- rep(0, num_farms) # aHerd$taggedDur
  # New additions TH
  farm_info$detected_tagged_dep <- rep(FALSE, num_farms) #aHerd$DetTaggedDep
  farm_info$time_visited <- rep(0, num_farms) #aHerd$timeVisited
  farm_info$immune_time <- rep(0, num_farms) #aHerd$immuneTime
  farm_info$time_culled <- rep(0, num_farms) #aHerd$timeCulled
  farm_info$to_surv_dead <- rep(0, num_farms) #aHerd$toSurvDead
  farm_info$dead_sample_test <- rep(0, num_farms) #aHerd$DeadSampTest
  farm_info$sampled_dead <- rep(0, num_farms) #aHerd$SampledDead
  farm_info$diagnosed_surveyed_dead <- rep(FALSE, num_farms) #aHerd$DiagSurvDead
  farm_info$sub_dead_sample <- rep(0, num_farms) #aHerd$SubDeadSamp
  farm_info$susceptible_again <- rep(0, num_farms) #aHerd$SusAgain
  farm_info$survived <- rep(0, num_farms) #aHerd$Survived
  farm_info$in_surveillance_zone <- rep(0, num_farms) #aHerd$inSurZone

  # New variables TA and DP
  environment$farm_info$infectiousness <- rep(0, num_farms) #aHerd$Infectiousness
  environment$farm_info$infection_mode <- rep(0, num_farms) #aHerd$infMode
  environment$farm_info$netpen_size_variable <- farm_info$netpen_size #aHerd$CageSizeVar
  environment$farm_info$netpen_size_cull <- farm_info$netpen_size #aHerd$CageSizeCull
  environment$farm_info$diagnosis_mode <- rep(0, num_farms) #aHerd$DiagMode
  environment$farm_info$time_to_first_surveillance_visit <- rep(0, num_farms) #aHerd$timeToFSV
  environment$farm_info$time_to_subsequent_surveillance_visit <- rep(0, num_farms) #aHerd$timeToSSV
  environment$farm_infO$time_to_asv <- rep(0, num_farms) #aHerd$timeToASV
  environment$farm_infO$farm_diagnosis <- rep(FALSE, num_farms) #aHerd$FarmDiag
  # number of samples for surveillance - to be checked by DP
  environment$farm_info$number_of_samples <- round(rtriang(num_farms, 5, 10, 20)) #aHerd$NumSamp
  environment$farm_info$number_of_samples_surveyed <- rep(0, num_farms) #aHerd$NumSamSurv
  environment$farm_info$farm_size_surveyed <- rep(0, num_farms) #aHerd$herdSizeSurv
  environment$farm_info$visit_count <- rep(0, num_farms) #aHerd$visitCount
  farm_a_s_visit <- round(runif(unique(environment$farm_info$farm_id)), 1, 90)
  environment$farm_info$time_to_asv <- farm_a_s_visit[environment$farm_info$farm_id]

  # iterate over species to set waiting periods and transmission probabilities
  # by species
  for (id in species_info$species_id) {
    farm_index <- environment$farm_info$species_id == id
    num_type <- sum(farm_index)
    species_index <- environment$species_info$species_id
    # intra-farm interaction rate
    # TODO
    # Assign k value to farm based on k value of species
    environment$farm_info$k[farm_index] <- species_info$k[species_index]
    # Assign relative susceptibility to farm based on relative susceptibility
    # of species
    environment$farm_info$rel_susceptibility[farm_index] <-
      environment$species_info$rel_susceptibility[species_index]
  }

  # assign reed-frost probability?
  environment$farm_info$p <- environment$farm_info$k
  environment$farm_info$p[environment$farm_info$p > 1] <- 1

  farm_info <- he_reset_simulation_env(environment,
                                       environment$num_farms,
                                       environment$farm_info)
  # move into simulation reset?
  farm_info$status <- init_status
  farm_info$time_infected <- init_time_infected

  # select index herds for next simulation?
  if (ignore_status) {
    # TODO: Select index farm based on index farm selection function
    environment$index_farm <- select_index_herd()
    # apply infected status to the index farm
    farm_info$status[environment$index_farm]
    farm_info$time_infected[environment$index_farm] <- 0

    # TODO: Confirm OK to skip "step-in file" section
    if (index_direct) {
      # TODO: construct an infected farm
      # aInfHerd$addInf(indexHerd,
      #                 matrix(c(0, 1, 0),
      #                        byrow = TRUE,
      #                        ncol = 3,
      #                        nrow = length(indexHerd)),
      #                 1)
    } else {
      # TODO: construct an infected farm
      # aInfHerd$addInf(indexHerd,
      #                 matrix(c(1, 0, 0),
      #                        byrow = TRUE,
      #                        ncol = 3,
      #                        nrow = length(indexHerd)),
      #                 0)
    }
  }
  # TODO: What are these?
  # environment$vaccinated_today <- 0
  # environment$lock <- FALSE

  # TODO: Review these variables - what are these bay management area vars for?
  temp_bay_management <- round(runif(unique(farm_info$bay_management_id)))
  farm_info$bay_management_time <- rep(0, num_farms)
  farm_info$productive_time <- rep(0, num_farms)
  farm_info$farm_active <- rep(FALSE, num_farms)
  farm_info$bay_management_time <- temp_bay_management[farm_info$bay_management_id]
  farm_info$productive_time <-
    farm_info$bay_management_time - round(runif(length(farm_info$bay_management_time), 0, 120))

  # Set the index farm to be active
  index_farm_id <- farm_info$farm_id[index_farm]
  farm_info$productive_time[farm_info$farm_id %in% index_farm_id] <-
    round(runif(sum(farm_info$farm_id %in% index_farm_id), 0, 120))
  farm_info$productive_time[index_farm]

  farm_info
}
