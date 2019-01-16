library(offsetsim)

source_filename = 'cumberland_params_scenario_5.R'
source(source_filename)

user_simulation_params = initialise_user_simulation_params()
user_global_params = initialise_user_global_params()
user_output_params <- initialise_user_output_params()

# author_names = c('REVISED_Elicitation_CP_Workshop_dkirk_splines.rds', 
#                  'REVISED_Elicitation_CP_Workshop_dkeith_splines.rds', 
#                  'REVISED_Elicitation_CP_Workshop_gsteenbeeke_splines.rds', 
#                  'REVISED_Elicitation_CP_Workshop_pprice_splines.rds', 
#                  'REVISED_Elicitation_CP_Workshop_cmorris_splines.rds')

author_names = 'mean_splines.rds'
author_sheet_data = lapply(seq_along(author_names), 
                                   function(i) readRDS(author_names[i])) 

for (i in seq_along(author_names)){
  
  user_feature_params = initialise_user_feature_params(author_sheet_data[i])
  osim.run(user_global_params, user_simulation_params, user_feature_params, user_transform_function, loglevel = 'TRACE')
  simulation_folder = find_current_run_folder(user_global_params$simulation_folder)
  osim.output(user_output_params, simulation_folder)
  paste0('osim.run() completed for author ', i, '- generating outputs')
  
}

cat('\n simulation and outputs using', source_filename, 'completed \n')