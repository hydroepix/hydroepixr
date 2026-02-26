utils::globalVariables(
  c(
    # expected columns in data frames
    'within_net_pen_transmission_min',
    'within_net_pen_transmission_mode',
    'within_net_pen_transmission_max',
    "species_id",
    'infection_stage',
    "farm_id",
    "n_susceptible",
    "n_latent",
    "n_subclinical",
    "n_clinical",
    "n_recovered",
    "n_dead",
    "simulation_num",
    "net_pen_id",

    # expected variables from another environment
    'simulation_day',

    # plotting
    'value'
  )
)
