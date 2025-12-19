#' Add infected net pen to infected net pen tracking
#'
#' @param infected_net_pen_info data frame of information on infected net_pens
#' @param net_pen_info data frame of information on net_pens
#' @param new_infected_net_pens list of newly infected net pen ids
#' @param n_infected_animals_by_stage vector of numeric values indicating the
#'    number of infected animals in each stage of infection (latent, clinical,
#'    and subclinical)
#' @param infection_origin source of the infection can be "index",
#'    "between-net_pen", or "between-farm"
#' @param simulation_day day of simulation that infection is occurring
#'
#' @return data frame of information on infected net_pens, appended with rows to
#'    for new infections
#' @export
#'
he_add_infected_net_pen <-
  function(
    infected_net_pen_info,
    net_pen_info,
    new_infected_net_pens,
    n_infected_animals_by_stage,
    infection_origin,
    simulation_day
  ) {
    # Filter new infection ids to remove those already infected
    already_infected_net_pens <- new_infected_net_pens %in%
      infected_net_pen_info$net_pen_id
    if (any(already_infected_net_pens)) {
      new_infected_net_pens <- new_infected_net_pens[!already_infected_net_pens]
      n_infected_animals_by_stage <-
        n_infected_animals_by_stage[!already_infected_net_pens, , drop = FALSE]
      infection_origin <- infection_origin[!already_infected_net_pens]
    }

    # If there are new infections left to add
    if (length(new_infected_net_pens) > 0) {
      new_infected_net_pen_rows <-
        data.frame(
          simulation_day = simulation_day,
          net_pen_id = new_infected_net_pens,
          farm_id = net_pen_info$farm_id[new_infected_net_pens],
          species_id = net_pen_info$species_id[new_infected_net_pens],
          within_net_pen_transmission = net_pen_info$within_net_pen_transmission[
            new_infected_net_pens
          ],
          n_susceptible = net_pen_info$net_pen_size[new_infected_net_pens] -
            rowSums(n_infected_animals_by_stage),
          n_infected_animals_by_stage,
          n_recovered = 0,
          n_dead = 0,
          n_total = net_pen_info$net_pen_size[new_infected_net_pens],
          infection_origin = infection_origin,
          day_infected = simulation_day,
          is_vaccinated = 0
        )

      # Row names needs to be false due to the match condition above that checks
      # for already infected net_pens - it seems to convert row names to
      # character vectors instead of numeric values
      infected_net_pen_info <-
        rbind(
          infected_net_pen_info,
          new_infected_net_pen_rows,
          make.row.names = FALSE
        )
    }
    infected_net_pen_info
  }
