he_plot_simulation_examples <- function(
  sim_dat,
  simulation_nums = NULL,
  infection_stages = NULL
) {
  if (length(simulation_nums) > 5) {
    warning(
      "Warning: Plotting more than 5 simulations will generate a very large number of plots, since a plot is generated for each infection stage and each simulation number. This is not recommended as the plots become very small and may take a long time to generate."
    )
  }

  # Set theme
  ggplot2::theme_set(ggplot2::theme_light())

  # Define intfection columns and set levels for a hierarchy
  infection_cols <- c(
    "n_susceptible",
    "n_latent",
    "n_subclinical",
    "n_clinical",
    "n_recovered",
    "n_dead"
  )
  infection_levels <- gsub("n_", "", infection_cols)

  # Reformat data for plotting
  dat <- sim_dat |>
    dplyr::select(
      simulation_num,
      simulation_day,
      net_pen_id,
      farm_id,
      species_id,
      n_susceptible,
      n_latent,
      n_subclinical,
      n_clinical,
      n_recovered,
      n_dead
    ) |>
    tidyr::pivot_longer(
      cols = n_susceptible:n_dead,
      values_to = "value",
      names_to = "infection_stage"
    ) |>
    dplyr::mutate(
      infection_stage = gsub("n_", "", infection_stage),
      infection_stage = ordered(infection_stage, levels = infection_levels)
    )

  # Filtering by simulation number, sampling randomly if unspecified
  if (is.null(simulation_nums)) {
    # Check how many simulations are included and randomly sample 5
    num_simulations <- length(unique(dat$simulation_num))
    simulation_nums <- sample(
      unique(dat$simulation_num),
      # If there are fewer than 5, we should just include them all
      size = if (num_simulations < 5) num_simulations else 5
    )
  }
  dat <- dat |> dplyr::filter(simulation_num %in% simulation_nums)

  # Filtering by infection stage, if relevant
  if (!is.null(infection_stages)) {
    # Filter out any infection stages not listed
    dat <- dat |>
      dplyr::filter(infection_stage %in% infection_stages)
    # Infection levels should also be filtered
    infection_levels <- infection_levels %in% infection_stages
  }

  # Order variables
  # Must be done after filtering to avoid including filtered out levels
  dat <- dat |>
    dplyr::mutate(
      infection_stage = gsub("n_", "", infection_stage),
      infection_stage = ordered(
        infection_stage,
        levels = infection_levels
      ),
      net_pen_id = ordered(net_pen_id),
      simulation_num = ordered(simulation_num)
    )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(simulation_day, value, group = net_pen_id, colour = net_pen_id)
  ) +
    ggplot2::geom_line(linewidth = 1, show.legend = TRUE) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_y_continuous("Number of Individuals") +
    ggplot2::scale_x_continuous("Simulation Day") +
    ggplot2::facet_grid(
      rows = dplyr::vars(infection_stage),
      cols = dplyr::vars(simulation_num)
    )
}
