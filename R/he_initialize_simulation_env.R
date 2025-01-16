he_initialize_simulation_env <- function(environment, num_farms) {
  # gDaysUntilBaseline - determine the day the first detection will happen
  environment$days_until_baseline_mort <- 0

  # TotNumQueue - output variable to calculate the number of "herds" queueing
  # for surveillance per day
  # does not affect anything (TH) ?
  environment$total_num_queued <- 0

  # Initialize waiting periods and transmission probabilities
  environment$farm_info$tagged_duration <- rep(0, num_farms) # aHerd$taggedDur
  # New additions TH
  environment$farm_info$detected_tagged_dep <- rep(FALSE, num_farms) #aHerd$DetTaggedDep
  environment$farm_info$time_visited <- rep(0, num_farms) #aHerd$timeVisited
  environment$farm_info$immune_time <- rep(0, num_farms) #aHerd$immuneTime
  environment$farm_info$time_culled <- rep(0, num_farms) #aHerd$timeCulled
  environment$farm_info$to_surv_dead <- rep(0, num_farms) #aHerd$toSurvDead
  environment$farm_info$dead_sample_tes <- rep(0, num_farms) #aHerd$DeadSampTest
  environment$farm_info$sampled_dead <- rep(0, num_farms) #aHerd$SampledDead
  environment$farm_info$diagnosed_surveyed_dead <- rep(FALSE, num_farms) #aHerd$DiagSurvDead
  environment$farm_info$sub_dead_sample <- rep(0, num_farms) #aHerd$SubDeadSamp
  environment$farm_info$susceptible_again <- rep(0, num_farms) #aHerd$SusAgain
  environment$farm_info$survived <- rep(0, num_farms) #aHerd$Survived
  environment$farm_info$in_surveillance_zone <- rep(0, num_farms) #aHerd$inSurZone

  # New variables TA and DP
  environment$farm_info$infectiousness <- rep(0, num_farms) #aHerd$Infectiousness
  environment$farm_info$infection_mode <- rep(0, num_farms) #aHerd$infMode
  environment$farm_info$cage_size_variable <- farm_info$cage_size #aHerd$CageSizeVar
  environment$farm_info$cage_size_cull <- farm_info$cage_size #aHerd$CageSizeCull
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



}
