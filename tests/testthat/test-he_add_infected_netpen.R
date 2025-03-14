test_that("single netpen is correctly appended to empty infected farm info", {
  test_inf_farm_info <- readRDS(paste0(test_data_filepath,
                                       "/inf_farm_info_initialized.rds"))
  test_farm_info <- readRDS(paste0(test_data_filepath,
                                   "/initialized_farm_info_bay_x.rds"))
  test_netpen_ids_to_infect <- 4
  test_num_inf_animals_by_stage <- data.frame(
    latent = 1,
    subclinical = 0,
    clinical = 0
  )
  test_type_of_contact <- "direct"
  test_sim_timestep <- 1

  expected_inf_farm_info <- data.frame(
    netpen_id = 4,
    farm_id = 1,
    species_id = 1,
    within_netpen_transmission = 0.55820072,
    susceptible = 24999,
    latent = 1,
    subclinical = 0,
    clinical = 0,
    immune = 0,
    total = 25000,
    infection_status = 1,
    latent_duration = 0,
    subclinical_duration = 0,
    clinical_time = Inf,
    time_of_diagnosis = Inf,
    diagnosed = 0,
    infected_by_direct_contact = "direct",
    time_infected = 1,
    vaccinated = 0
  )


  result_inf_farm_info <-
    he_add_infected_netpen(test_inf_farm_info,
                           test_farm_info,
                           test_netpen_ids_to_infect,
                           test_num_inf_animals_by_stage,
                           test_type_of_contact,
                           test_sim_timestep)

  expect_equal(result_inf_farm_info, expected_inf_farm_info)
})

test_that("netpens are correctly appended to empty infected farm info", {
  test_inf_farm_info <- readRDS(paste0(test_data_filepath,
                                       "/inf_farm_info_initialized.rds"))
  test_farm_info <- readRDS(paste0(test_data_filepath,
                                   "/initialized_farm_info_bay_x.rds"))
  test_netpen_ids_to_infect <- c(4, 21)
  test_num_inf_animals_by_stage <- data.frame(
    latent = c(1, 0),
    subclinical = c(0, 1),
    clinical = c(0, 0)
  )
  test_type_of_contact <- c("indirect", "direct")
  test_sim_timestep <- 1

  expected_inf_farm_info <- data.frame(
    netpen_id = c(4, 21),
    farm_id = c(1, 2),
    species_id = c(1, 1),
    within_netpen_transmission = c(0.55820072, 0.56227532),
    susceptible = c(24999, 24999),
    latent = c(1, 0),
    subclinical = c(0, 1),
    clinical = c(0, 0),
    immune = c(0,0),
    total = c(25000, 25000),
    infection_status = c(1, 1), # ignored for now
    latent_duration = c(0, 0),
    subclinical_duration = c(0, 0),
    clinical_time = c(Inf, Inf),
    time_of_diagnosis = c(Inf, Inf),
    diagnosed = c(0, 0),
    infected_by_direct_contact = c("indirect", "direct"),
    time_infected = c(1, 1),
    vaccinated = c(0, 0)
  )

  result_inf_farm_info <-
    he_add_infected_netpen(test_inf_farm_info,
                           test_farm_info,
                           test_netpen_ids_to_infect,
                           test_num_inf_animals_by_stage,
                           test_type_of_contact,
                           test_sim_timestep)

  expect_equal(result_inf_farm_info, expected_inf_farm_info)
})

test_that("single netpen is correctly appended to infected farm info", {
  test_inf_farm_info <- readRDS(
    paste0(
      test_data_filepath,
      "/inf_farm_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_farm_info <- readRDS(paste0(test_data_filepath,
                                   "/initialized_farm_info_bay_x.rds"))
  test_netpen_ids_to_infect <- 5
  test_num_inf_animals_by_stage <- data.frame(
    latent = 5,
    subclinical = 0,
    clinical = 0
  )
  test_type_of_contact <- "indirect"
  test_sim_timestep <- 10

  expected_inf_farm_info <-
    rbind(
      test_inf_farm_info,
      data.frame(
        netpen_id = 5,
        farm_id = 1,
        species_id = 1,
        within_netpen_transmission =  0.48065623,
        susceptible = 24995,
        latent = 5,
        subclinical = 0,
        clinical = 0,
        immune = 0,
        total = 25000,
        infection_status = 1,
        latent_duration = 0,
        subclinical_duration = 0,
        clinical_time = Inf,
        time_of_diagnosis = Inf,
        diagnosed = 0,
        infected_by_direct_contact = "indirect",
        time_infected = 10,
        vaccinated = 0
      )
    )


  result_inf_farm_info <-
    he_add_infected_netpen(test_inf_farm_info,
                           test_farm_info,
                           test_netpen_ids_to_infect,
                           test_num_inf_animals_by_stage,
                           test_type_of_contact,
                           test_sim_timestep)

  expect_equal(result_inf_farm_info, expected_inf_farm_info)
})

test_that("netpens are correctly appended to infected farm info", {
  test_inf_farm_info <- readRDS(
    paste0(
      test_data_filepath,
      "/inf_farm_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_farm_info <- readRDS(paste0(test_data_filepath,
                                   "/initialized_farm_info_bay_x.rds"))
  test_netpen_ids_to_infect <- c(5, 6)
  test_num_inf_animals_by_stage <- data.frame(
    latent = c(10, 5),
    subclinical = c(0, 0),
    clinical = c(0, 0)
  )
  test_type_of_contact <- c("direct", "direct")
  test_sim_timestep <- 12

  expected_inf_farm_info <- rbind(
    test_inf_farm_info,
    data.frame(
      netpen_id = c(5, 6),
      farm_id = c(1, 1),
      species_id = c(1, 1),
      within_netpen_transmission = c(0.48065623, 0.45983893),
      susceptible = c(24990, 24995),
      latent = c(10, 5),
      subclinical = c(0, 0),
      clinical = c(0, 0),
      immune = c(0, 0),
      total = c(25000, 25000),
      infection_status = c(1, 1),
      # ignored for now
      latent_duration = c(0, 0),
      subclinical_duration = c(0, 0),
      clinical_time = c(Inf, Inf),
      time_of_diagnosis = c(Inf, Inf),
      diagnosed = c(0, 0),
      infected_by_direct_contact = c("direct", "direct"),
      time_infected = c(12, 12),
      vaccinated = c(0, 0)
    )
  )

  result_inf_farm_info <-
    he_add_infected_netpen(test_inf_farm_info,
                           test_farm_info,
                           test_netpen_ids_to_infect,
                           test_num_inf_animals_by_stage,
                           test_type_of_contact,
                           test_sim_timestep)

  expect_equal(result_inf_farm_info, expected_inf_farm_info)
})

test_that("netpens are not appended when they already exist in infected farm info", {
  test_inf_farm_info <- readRDS(
    paste0(
      test_data_filepath,
      "/inf_farm_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_farm_info <- readRDS(paste0(test_data_filepath,
                                   "/initialized_farm_info_bay_x.rds"))
  test_netpen_ids_to_infect <- c(4, 21)
  test_num_inf_animals_by_stage <- data.frame(
    latent = c(1, 0),
    subclinical = c(0, 1),
    clinical = c(0, 0)
  )
  test_type_of_contact <- c("indirect", "direct")
  test_sim_timestep <- 1

  expected_inf_farm_info <- data.frame(
    netpen_id = c(4, 21),
    farm_id = c(1, 2),
    species_id = c(1, 1),
    within_netpen_transmission = c(0.55820072, 0.56227532),
    susceptible = c(24999, 24999),
    latent = c(1, 0),
    subclinical = c(0, 1),
    clinical = c(0, 0),
    immune = c(0,0),
    total = c(25000, 25000),
    infection_status = c(1, 1), # ignored for now
    latent_duration = c(0, 0),
    subclinical_duration = c(0, 0),
    clinical_time = c(Inf, Inf),
    time_of_diagnosis = c(Inf, Inf),
    diagnosed = c(0, 0),
    infected_by_direct_contact = c("indirect", "direct"),
    time_infected = c(1, 1),
    vaccinated = c(0, 0)
  )

  result_inf_farm_info <-
    he_add_infected_netpen(test_inf_farm_info,
                           test_farm_info,
                           test_netpen_ids_to_infect,
                           test_num_inf_animals_by_stage,
                           test_type_of_contact,
                           test_sim_timestep)

  expect_equal(result_inf_farm_info, expected_inf_farm_info)
})

test_that("single netpen is correctly appended when already infected netpens are
          filtered out", {
  test_inf_farm_info <- readRDS(
    paste0(
      test_data_filepath,
      "/inf_farm_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_farm_info <- readRDS(paste0(test_data_filepath,
                                   "/initialized_farm_info_bay_x.rds"))
  test_netpen_ids_to_infect <- c(4, 21, 5)
  test_num_inf_animals_by_stage <- data.frame(
    latent = c(1, 1, 1),
    subclinical = c(0, 0, 0),
    clinical = c(0, 0, 0)
  )
  test_type_of_contact <- c("direct", "direct", "direct")
  test_sim_timestep <- 5

  expected_inf_farm_info <- rbind(
    test_inf_farm_info,
    data.frame(
      netpen_id = 5,
      farm_id = 1,
      species_id = 1,
      within_netpen_transmission = 0.48065623,
      susceptible = 24999,
      latent = 1,
      subclinical = 0,
      clinical = 0,
      immune = 0,
      total = 25000,
      infection_status = 1,
      # ignored for now
      latent_duration = 0,
      subclinical_duration = 0,
      clinical_time = Inf,
      time_of_diagnosis = Inf,
      diagnosed = 0,
      infected_by_direct_contact = "direct",
      time_infected = 5,
      vaccinated = 0
    )
  )

  result_inf_farm_info <-
    he_add_infected_netpen(test_inf_farm_info,
                           test_farm_info,
                           test_netpen_ids_to_infect,
                           test_num_inf_animals_by_stage,
                           test_type_of_contact,
                           test_sim_timestep)

  expect_equal(result_inf_farm_info, expected_inf_farm_info)
})

test_that("multiple netpens are correctly appended when already infected netpens
          are filtered out", {
  test_inf_farm_info <- readRDS(
    paste0(
      test_data_filepath,
      "/inf_farm_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_farm_info <- readRDS(paste0(test_data_filepath,
                                   "/initialized_farm_info_bay_x.rds"))
  test_netpen_ids_to_infect <- c(4, 21, 5, 6)
  test_num_inf_animals_by_stage <- data.frame(
    latent = c(1, 1, 1, 1),
    subclinical = c(0, 0, 0, 0),
    clinical = c(0, 0, 0, 0)
  )
  test_type_of_contact <- c("direct", "direct", "direct", "direct")
  test_sim_timestep <- 5

  expected_inf_farm_info <- rbind(
    test_inf_farm_info,
    data.frame(
      netpen_id = c(5, 6),
      farm_id = c(1, 1),
      species_id = c(1, 1),
      within_netpen_transmission = c(0.48065623, 0.45983893),
      susceptible = c(24999, 24999),
      latent = c(1, 1),
      subclinical = c(0, 0),
      clinical = c(0, 0),
      immune = c(0, 0),
      total = c(25000, 25000),
      infection_status = c(1, 1),
      # ignored for now
      latent_duration = c(0, 0),
      subclinical_duration = c(0, 0),
      clinical_time = c(Inf, Inf),
      time_of_diagnosis = c(Inf, Inf),
      diagnosed = c(0, 0),
      infected_by_direct_contact = c("direct", "direct"),
      time_infected = c(5, 5),
      vaccinated = c(0, 0)
    )
  )

  result_inf_farm_info <-
    he_add_infected_netpen(test_inf_farm_info,
                           test_farm_info,
                           test_netpen_ids_to_infect,
                           test_num_inf_animals_by_stage,
                           test_type_of_contact,
                           test_sim_timestep)

  expect_equal(result_inf_farm_info, expected_inf_farm_info)
})
