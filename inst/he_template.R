model_env <- he_create_model_env()

he_define_simulation_control_params(model_env,
                                    n_simulations = 1,
                                    max_outbreak_length = 365,
                                    random_seed = -10,
                                    ignore_disease_status_input = TRUE,
                                    verbose = FALSE)
he_define_spread_control_params(model_env,
                                n_index_infected_min = 50,
                                n_index_infected_mode = 50,
                                n_index_infected_max = 50,
                                index_netpen_ids = 1,
                                index_farm_id = NULL,
                                index_direct = TRUE,
                                clinically_infected_prop = 0.9,
                                days_dead_infectious = 2,
                                farm_to_farm = 0.5,
                                netpen_to_netpen = 0.05,
                                vaccine_efficacy = 0)
he_define_output_params(model_env,
                        output_dir = "output",
                        model_run_id = "year-long-low-within-netpen-transmission",
                        infected_netpen_output_file_name = "infected_netpens.csv")

# Set up model environment based on data files and parameters
he_initialize_model_env(model_env,
                        netpen_info_filepath = "C:/Users/Rachel Woodside/GitHub Repos/hydroepixr/inst/testdata/netpen_file_bay_x.csv",
                        species_info_filepath = "C:/Users/Rachel Woodside/GitHub Repos/hydroepixr/inst/testdata/species_info_file_bay_x_low-within-netpen-transmission.csv"
                        #connectivity_matrix_filepath = "../../../../testdata/dist_mat_bay_x.csv"
)

he_run_model(model_env)
