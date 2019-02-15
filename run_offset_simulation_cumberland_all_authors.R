library(offsetsim)

scenario_num = 6

source('global_scenario_params_set.R')
user_output_params <- initialise_user_output_params()
user_simulation_params_global <- initialise_user_simulation_params_global()
user_feature_params_global <- initialise_user_feature_params_global()

source_filename <- paste0('cumberland_params_scenario_', scenario_num, '.R')
source(source_filename)


# Note: if changing to other authors you need to make sure to set 
# global_params$overwrite_feature_dynamics = TRUE in global_scenario_params_set.R

# Also note that when changing authors, if you are include background
# different background dynamics of conservation areas (eg biobank high
# management, conservation areas low management), then Isaac neeads to
# manually generate the feature_dynamics.rds file for each case. So without
# this being done, you can only run with no management of conservation areas
# and biobank sites.

#author_names = c('dkirk', 'dkeith', 'gsteenbeeke', 'pprice', 'cmorris')
author_names = c( 'mean' )

# This determins whether managemetn: 1=no management, 2=low intensity management, 3=high intensity management
management_index_set = c(1, 1, 2, 2, 3, 3)

author_sheet_data = lapply(seq_along(author_names), 
                                   function(i) readRDS(paste0('REVISED_Elicitation_CP_Workshop_', author_names[i], '_splines.rds'))) 

for (i in seq_along(author_names)){

  user_feature_params = initialise_feature_dynamics_set(user_feature_params_global, 
                                                        current_author_splines = author_sheet_data[[i]], 
                                                        management_index_to_use = management_index_set[scenario_num])
  
  user_global_params = initialise_user_global_params(folder_to_use = paste0(author_names[i], '/scenario_', scenario_num))
  user_simulation_params = initialise_user_simulation_params(user_simulation_params_global, user_global_params$time_steps)
  
  osim.run(user_global_params, user_simulation_params, user_feature_params, user_transform_function, loglevel = 'TRACE')
  simulation_folder = find_current_run_folder(user_global_params$simulation_folder)
  osim.output(user_output_params, simulation_folder)
  paste0('osim.run() completed for author ', i, '- generating outputs')
  
}

cat('\n simulation and outputs using', source_filename, 'completed \n')