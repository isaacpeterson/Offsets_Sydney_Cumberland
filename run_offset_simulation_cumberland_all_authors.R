library(offsetsim)

source_filename = 'cumberland_params_scenario_5.R'
source(source_filename)

user_simulation_params = initialise_user_simulation_params()
user_global_params = initialise_user_global_params()
user_output_params <- initialise_user_output_params()

author_names = c('dkirk', 'dkeith', 'gsteenbeeke', 'pprice', 'cmorris')
author_sheet_data = lapply(seq_along(author_names), 
                                   function(i) readRDS(paste0('REVISED_Elicitation_CP_Workshop_', 
                                                              author_names[i], '_splines.rds'))) 

for (i in seq_along(author_names)){
  
  user_feature_params = initialise_user_feature_params(author_sheet_data[i])
  osim.run(user_global_params, user_simulation_params, user_feature_params, user_transform_function, loglevel = 'TRACE')
  simulation_folder = find_current_run_folder(user_global_params$simulation_folder)
  osim.output(user_output_params, simulation_folder)
  paste0('osim.run() completed for author ', i, '- generating outputs')
  
}

cat('\n simulation and outputs using', source_filename, 'completed \n')