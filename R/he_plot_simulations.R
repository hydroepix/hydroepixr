# library(dplyr)
# library(ggplot2)
# library(here)
# library(RColorBrewer)
# library(readr)
# library(tidyr)
#
#
# # theme_set(theme_light())
# #
# # infection_levels <- c("susceptible", "latent", "subclinical", "clinical", "immune")
#
#
# dat_raw <- read_csv(
#   here("output/high-within-net-pen-transmission_1_infected_net-pens.csv"),
#   show_col_types = FALSE
# )
#
# dat_raw <- read_csv(
#   here("output/year-long-mid-within-net-pen-transmission_1_infected_net-pens.csv"),
#   show_col_types = FALSE
# )

# dat <- dat_raw %>%
#   select(
#     simulation_day, net_pen_id, farm_id, species_id,
#     n_susceptible,
#     n_latent, n_subclinical, n_clinical, n_immune
#   ) %>%
#   pivot_longer(
#     cols = n_susceptible:n_immune,
#     values_to = "value", names_to = "infection_stage"
#   ) %>%
#   mutate(
#     infection_stage = gsub("n_", "", infection_stage),
#     infection_stage = ordered(infection_stage, levels = infection_levels)
#   )# %>%
# filter(infection_stage != "subclinical")

#' Plot the number of individuals in each stage over the simulation
#'
#' Infection stages will have the same colour between plots, even if a stage(s)
#' is dropped.
#'
#' @param dat tbd
#'
#' @returns tbd
#'
#' @importFrom dplyr any_of
#' @importFrom ggplot2 aes ggplot geom_line scale_color_manual
#'   scale_x_continuous scale_y_continuous theme_light theme_set
#' @importFrom tidyr pivot_longer
#'
#' @export
#'

he_plot_simulation <- function(dat, pal = NULL) {
  theme_set(theme_light())

  if (is.null(pal)) {
    pal <- brewer.pal(5, "Dark2")
  }

  infection_cols <- c(
    "n_susceptible",
    "n_latent",
    "n_subclinical",
    "n_clinical",
    "n_immune"
  )
  infection_levels <- gsub("n_", "", infection_cols)

  if (!("infection_stage" %in% colnames(dat))) {
    dat <- dat %>%
      pivot_longer(
        cols = any_of(infection_cols),
        values_to = "value",
        names_to = "infection_stage"
      )
  }

  dat <- dat %>%
    mutate(
      infection_stage = gsub("n_", "", infection_stage),
      infection_stage = ordered(infection_stage, levels = infection_levels)
    )

  ggplot(
    dat,
    aes(
      simulation_day,
      value,
      colour = infection_stage,
      group = infection_stage
    )
  ) +
    geom_line(linewidth = 1, show.legend = TRUE) +
    scale_color_manual("Infection Stage", values = pal, drop = FALSE) +
    scale_y_continuous("Number of Individuals") +
    scale_x_continuous("Simulation Day")
}
