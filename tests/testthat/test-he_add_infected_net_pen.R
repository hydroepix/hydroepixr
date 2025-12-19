test_that("single net pen is correctly appended to empty infected net pen info", {
  test_infected_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/infected_net_pen_info_initialized.rds"
  ))
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_net_pen_ids_to_infect <- 4
  test_n_infected_animals_by_stage <- data.frame(
    n_latent = 1,
    n_subclinical = 0,
    n_clinical = 0
  )
  test_infection_origin <- "index"
  test_simulation_day <- 0

  expected_infected_net_pen_info <- data.frame(
    simulation_day = test_simulation_day,
    net_pen_id = 4,
    farm_id = 1,
    species_id = 1,
    within_net_pen_transmission = 0.55820072,
    n_susceptible = 24999,
    n_latent = 1,
    n_subclinical = 0,
    n_clinical = 0,
    n_recovered = 0,
    n_dead = 0,
    n_total = 25000,
    infection_origin = test_infection_origin,
    day_infected = 0,
    is_vaccinated = 0
  )

  result_infected_net_pen_info <-
    he_add_infected_net_pen(
      test_infected_net_pen_info,
      test_net_pen_info,
      test_net_pen_ids_to_infect,
      test_n_infected_animals_by_stage,
      test_infection_origin,
      test_simulation_day
    )

  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
})

test_that("net pens are correctly appended to empty infected net pen info", {
  test_infected_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/infected_net_pen_info_initialized.rds"
  ))
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_net_pen_ids_to_infect <- c(4, 21)
  test_n_infected_animals_by_stage <- data.frame(
    n_latent = c(1, 0),
    n_subclinical = c(0, 1),
    n_clinical = c(0, 0)
  )
  test_infection_origin <- c("index", "index")
  test_simulation_day <- 0

  expected_infected_net_pen_info <- data.frame(
    simulation_day = test_simulation_day,
    net_pen_id = c(4, 21),
    farm_id = c(1, 2),
    species_id = c(1, 1),
    within_net_pen_transmission = c(0.55820072, 0.56227532),
    n_susceptible = c(24999, 24999),
    n_latent = c(1, 0),
    n_subclinical = c(0, 1),
    n_clinical = c(0, 0),
    n_recovered = c(0, 0),
    n_dead = c(0, 0),
    n_total = c(25000, 25000),
    infection_origin = c("index", "index"),
    day_infected = c(0, 0),
    is_vaccinated = c(0, 0)
  )

  result_infected_net_pen_info <-
    he_add_infected_net_pen(
      test_infected_net_pen_info,
      test_net_pen_info,
      test_net_pen_ids_to_infect,
      test_n_infected_animals_by_stage,
      test_infection_origin,
      test_simulation_day
    )

  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
})

test_that("single net pen is correctly appended to infected net pen info", {
  test_infected_net_pen_info <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_net_pen_ids_to_infect <- 5
  test_n_infected_animals_by_stage <- data.frame(
    n_latent = 5,
    n_subclinical = 0,
    n_clinical = 0
  )
  test_infection_origin <- "between-net-pen"
  test_simulation_day <- 10

  expected_infected_net_pen_info <-
    rbind(
      test_infected_net_pen_info,
      data.frame(
        simulation_day = test_simulation_day,
        net_pen_id = 5,
        farm_id = 1,
        species_id = 1,
        within_net_pen_transmission = 0.48065623,
        n_susceptible = 24995,
        n_latent = 5,
        n_subclinical = 0,
        n_clinical = 0,
        n_recovered = 0,
        n_dead = 0,
        n_total = 25000,
        infection_origin = test_infection_origin,
        day_infected = 10,
        is_vaccinated = 0
      )
    )

  result_infected_net_pen_info <-
    he_add_infected_net_pen(
      test_infected_net_pen_info,
      test_net_pen_info,
      test_net_pen_ids_to_infect,
      test_n_infected_animals_by_stage,
      test_infection_origin,
      test_simulation_day
    )

  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
})

test_that("net pens are correctly appended to infected net pen info", {
  test_infected_net_pen_info <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_net_pen_ids_to_infect <- c(5, 6)
  test_n_infected_animals_by_stage <- data.frame(
    n_latent = c(10, 5),
    n_subclinical = c(0, 0),
    n_clinical = c(0, 0)
  )
  test_infection_origin <- c("between-net-pen", "between-net-pen")
  test_simulation_day <- 12

  expected_infected_net_pen_info <- rbind(
    test_infected_net_pen_info,
    data.frame(
      simulation_day = test_simulation_day,
      net_pen_id = c(5, 6),
      farm_id = c(1, 1),
      species_id = c(1, 1),
      within_net_pen_transmission = c(0.48065623, 0.45983893),
      n_susceptible = c(24990, 24995),
      n_latent = c(10, 5),
      n_subclinical = c(0, 0),
      n_clinical = c(0, 0),
      n_recovered = c(0, 0),
      n_dead = c(0, 0),
      n_total = c(25000, 25000),
      infection_origin = test_infection_origin,
      day_infected = c(12, 12),
      is_vaccinated = c(0, 0)
    )
  )

  result_infected_net_pen_info <-
    he_add_infected_net_pen(
      test_infected_net_pen_info,
      test_net_pen_info,
      test_net_pen_ids_to_infect,
      test_n_infected_animals_by_stage,
      test_infection_origin,
      test_simulation_day
    )

  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
})

test_that("net pens are not appended when they already exist in infected net pen info", {
  test_infected_net_pen_info <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_net_pen_ids_to_infect <- c(4, 21)
  test_n_infected_animals_by_stage <- data.frame(
    n_latent = c(1, 0),
    n_subclinical = c(0, 1),
    n_clinical = c(0, 0)
  )
  test_infection_origin <- c("between-net-pen", "between-net-pen")
  test_simulation_day <- 1

  expected_infected_net_pen_info <- data.frame(
    simulation_day = test_simulation_day,
    net_pen_id = c(4, 21),
    farm_id = c(1, 2),
    species_id = c(1, 1),
    within_net_pen_transmission = c(0.55820072, 0.56227532),
    n_susceptible = c(24999, 24999),
    n_latent = c(1, 0),
    n_subclinical = c(0, 1),
    n_clinical = c(0, 0),
    n_recovered = c(0, 0),
    n_dead = c(0, 0),
    n_total = c(25000, 25000),
    infection_origin = test_infection_origin,
    day_infected = c(1, 1),
    is_vaccinated = c(0, 0)
  )

  result_infected_net_pen_info <-
    he_add_infected_net_pen(
      test_infected_net_pen_info,
      test_net_pen_info,
      test_net_pen_ids_to_infect,
      test_n_infected_animals_by_stage,
      test_infection_origin,
      test_simulation_day
    )

  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
})

test_that("single net pen is correctly appended when already infected net pens are
          filtered out", {
  test_infected_net_pen_info <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_net_pen_ids_to_infect <- c(4, 21, 5)
  test_n_infected_animals_by_stage <- data.frame(
    n_latent = c(1, 1, 1),
    n_subclinical = c(0, 0, 0),
    n_clinical = c(0, 0, 0)
  )
  test_infection_origin <- rep("between-net-pen", 3)
  test_simulation_day <- 5

  expected_infected_net_pen_info <- rbind(
    test_infected_net_pen_info,
    data.frame(
      simulation_day = test_simulation_day,
      net_pen_id = 5,
      farm_id = 1,
      species_id = 1,
      within_net_pen_transmission = 0.48065623,
      n_susceptible = 24999,
      n_latent = 1,
      n_subclinical = 0,
      n_clinical = 0,
      n_recovered = 0,
      n_dead = 0,
      n_total = 25000,
      infection_origin = "between-net-pen",
      day_infected = 5,
      is_vaccinated = 0
    )
  )

  result_infected_net_pen_info <-
    he_add_infected_net_pen(
      test_infected_net_pen_info,
      test_net_pen_info,
      test_net_pen_ids_to_infect,
      test_n_infected_animals_by_stage,
      test_infection_origin,
      test_simulation_day
    )

  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
})

test_that("multiple net pens are correctly appended when already infected net pens
          are filtered out", {
  test_infected_net_pen_info <- readRDS(
    paste0(
      test_data_filepath,
      "/infected_net_pen_info_bay_x_with_multi_farm_infection.rds"
    )
  )
  test_net_pen_info <- readRDS(paste0(
    test_data_filepath,
    "/initialized_net_pen_info_bay_x.rds"
  ))
  test_net_pen_ids_to_infect <- c(4, 21, 5, 6)
  test_n_infected_animals_by_stage <- data.frame(
    n_latent = c(1, 1, 1, 1),
    n_subclinical = c(0, 0, 0, 0),
    n_clinical = c(0, 0, 0, 0)
  )
  test_infection_origin <- rep("between-net-pen", 4)
  test_simulation_day <- 5

  expected_infected_net_pen_info <- rbind(
    test_infected_net_pen_info,
    data.frame(
      simulation_day = test_simulation_day,
      net_pen_id = c(5, 6),
      farm_id = c(1, 1),
      species_id = c(1, 1),
      within_net_pen_transmission = c(0.48065623, 0.45983893),
      n_susceptible = c(24999, 24999),
      n_latent = c(1, 1),
      n_subclinical = c(0, 0),
      n_clinical = c(0, 0),
      n_recovered = c(0, 0),
      n_dead = c(0, 0),
      n_total = c(25000, 25000),
      infection_origin = rep("between-net-pen", 2),
      day_infected = c(5, 5),
      is_vaccinated = c(0, 0)
    )
  )

  result_infected_net_pen_info <-
    he_add_infected_net_pen(
      test_infected_net_pen_info,
      test_net_pen_info,
      test_net_pen_ids_to_infect,
      test_n_infected_animals_by_stage,
      test_infection_origin,
      test_simulation_day
    )

  expect_equal(result_infected_net_pen_info, expected_infected_net_pen_info)
})
