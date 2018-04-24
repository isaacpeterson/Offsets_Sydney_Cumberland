library(offsetsim)
source('initialise_params_sydney_cumberland.R')

user_simulation_params = initialise_user_simulation_params()
user_global_params = initialise_user_global_params()
osim.run(user_global_params, user_simulation_params, loglevel = 'TRACE')

user_output_params <- initialise_user_output_params()
current_simulation_folder = find_current_run_folder(user_global_params$simulation_folder)
osim.output(user_output_params,current_simulation_folder, loglevel = 'TRACE')