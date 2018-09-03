library(offsetsim)

source('cumberland_params.R')

user_simulation_params = initialise_user_simulation_params()
user_global_params = initialise_user_global_params()
user_feature_params = initialise_user_feature_params()
user_output_params <- initialise_user_output_params()

osim.run(user_global_params, user_simulation_params, user_feature_params, loglevel = 'TRACE')

simulation_folder = find_current_run_folder(user_global_params$simulation_folder, file_placeholder_width = 3)

osim.output(user_output_params, simulation_folder)