initialise_user_global_params <- function(folder_to_use){
  
  global_params = list()
  
  simulation_base_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/')
  #simulation_base_folder = '/mnt/offset_data/Sydney_Cumberland_Data/'
  global_params$simulation_folder = paste0(simulation_base_folder, folder_to_use, '/')
  
  global_params$feature_raster_files = paste0(simulation_base_folder, 'simulation_inputs_jan_17/', 
                                              (list.files(path = paste0(simulation_base_folder, 'simulation_inputs_jan_17/'),
                                                          pattern = 'PCT_849_feature_', all.files = FALSE, 
                                                          full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                                          include.dirs = FALSE, no.. = FALSE)))
  
  global_params$planning_units_raster = paste0(simulation_base_folder, 'simulation_inputs_jan_17/', 'cadastre_withconstraints.tif')
  
  global_params$condition_class_raster_files = paste0(simulation_base_folder, 'simulation_inputs_jan_17/', 
                                                      (list.files(path = paste0(simulation_base_folder, 'simulation_inputs_jan_17/'),
                                                                  pattern = 'PCT_849_condition_class_', all.files = FALSE, 
                                                                  full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                                                  include.dirs = FALSE, no.. = FALSE)))
  
  global_params$simulation_inputs_folder = paste0(simulation_base_folder, 'simulation_inputs_jan_17/')
  # What subset of features to use in the simulation
  # Need to keep these as is to use veg integrity score
  
  global_params$features_to_use_in_simulation = 1:5
  
  # How long to run the simulaton in years
  global_params$time_steps = 15 # 50
  
  global_params$store_zeros_as_sparse = TRUE
  
  # set to FALSE if running into memory troubles when collating
  global_params$collate_with_parallel_cores = TRUE
  # The number of cores to run on.
  global_params$number_of_cores = 'all'
  
  # The number of realizations to run
  global_params$realisation_num = 1
  
  global_params$save_simulation_outputs = TRUE
  
  # Builds site_characteristics object. Note for Cumberland analysis always
  # want this to be FALSE as this is built in initialise_cumberland_data.R
  global_params$overwrite_site_characteristics = FALSE
  
  global_params$run_from_simulated_data = FALSE
  
  global_params$save_simulation_outputs = TRUE
  
  # If these are set to TRUE, then every parcel will have an equal probability
  # of being developed and offset which you DON'T WANT if running development
  # in the PGAs. So these should be set to FALSE for if specifying dev and
  # offset areas in initialise_cumberland_data.R.
  global_params$overwrite_dev_probability_list = FALSE
  global_params$overwrite_offset_probability_list = FALSE
  global_params$overwrite_unregulated_probability_list = FALSE
  
  global_params$overwrite_site_features = FALSE
  
  # If building all inputs from scratch via the initialise_cumberland_data.R,
  # then these need to be set to TRUE for the first run, to generate the
  # appropriate R objects. For subsequent runs they can be set to FALSE to
  # save time. However it's not problem if they are left as TRUE, the run will
  # just take a bit longer to get started.
  
  global_params$overwrite_management_dynamics = TRUE
  global_params$overwrite_feature_dynamics = FALSE
  global_params$overwrite_condition_classes = FALSE
  global_params$build_background_cfacs = FALSE
  global_params$save_background_cfacs = TRUE
  
  return(global_params)
}
