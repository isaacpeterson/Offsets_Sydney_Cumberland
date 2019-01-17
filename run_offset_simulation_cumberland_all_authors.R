library(offsetsim)

scenario_num = 3

source(paste0('cumberland_params_scenario_', scenario_num, '.R'))

user_output_params <- initialise_user_output_params()

author_names = c('dkirk', 'dkeith', 'gsteenbeeke', 'pprice', 'cmorris')

# author_names = 'mean_splines.rds'
author_sheet_data = lapply(seq_along(author_names), 
                                   function(i) readRDS(paste0('REVISED_Elicitation_CP_Workshop_', author_names[i], '_splines.rds'))) 

for (i in seq_along(author_names)){
  
  user_feature_params = initialise_user_feature_params(author_sheet_data[[i]])
  folder_to_use = paste0(author_names[i], '/scenario_', scenario_num)
  user_global_params = initialise_user_global_params(folder_to_use)
  user_simulation_params = initialise_user_simulation_params(simulated_time_steps = user_global_params$time_steps)
  
  osim.run(user_global_params, user_simulation_params, user_feature_params, user_transform_function, loglevel = 'TRACE')
  simulation_folder = find_current_run_folder(user_global_params$simulation_folder)
  osim.output(user_output_params, simulation_folder)
  paste0('osim.run() completed for author ', i, '- generating outputs')
  
}

cat('\n simulation and outputs using', source_filename, 'completed \n')