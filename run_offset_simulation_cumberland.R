library(offsetsim)

source_filename = 'cumberland_params_scenario_5.R'
source(source_filename)

user_simulation_params = initialise_user_simulation_params()
user_global_params = initialise_user_global_params()
user_feature_params = initialise_user_feature_params()
user_output_params <- initialise_user_output_params()

osim.run(user_global_params, user_simulation_params, user_feature_params, user_transform_function, loglevel = 'TRACE')

cat('\nosim.run() completed - generating outputs\n')

simulation_folder = find_current_run_folder(user_global_params$simulation_folder)

osim.output(user_output_params, simulation_folder)

cat('\n simulation and outputs using', source_filename, 'completed \n')