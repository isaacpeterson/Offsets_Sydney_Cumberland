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


setup_sub_plots <- function(nx, ny, x_space, y_space){
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  par(mar = c(x_space, y_space, 1, 0), oma = c(2, 4, 2.5, 0.5))
  
  par(tcl = -0.25)
  par(mgp = c(2, 0.3, 0))
  
}


initialise_user_output_params <- function(){
  output_params = list()
  
  # if leave it as an empty vec will put in the the collated realizations
  # folder, otherwise you specify the path
  output_params$output_folder = vector() 
  output_params$output_type = 'csv' # set to 'raster', 'png', 'plot', or 'csv'
  
  output_params$write_pdf = TRUE
  
  output_params$plot_type = 'outcomes' # can be 'outcomes'  or 'impacts'
  
  # use 'all' for all or therwise the numern eg 6 means the first 6 realiztaions.
  
  output_params$realisation_num = 'all' # 'all' or number to plot
  
  # the dev-offset to plot for the site level results.
  output_params$sets_to_plot = 1
  output_params$plot_site = FALSE
  output_params$plot_program = TRUE
  output_params$plot_landscape = TRUE
  
  # if this is false won't generate plots of maps for the offset metric
  output_params$plot_offset_metric = TRUE
  
  output_params$scenario_vec = 'all' #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  
  # was originally done 
  output_params$plot_subset_type = 'all' #c('offset_action_type') # 'offset_calc_type', 'offset_action_type', offset_time_horizon'
  output_params$plot_subset_param = 'all' #c('maintain') # 'net_gains', 'restore', 15
  
  # Set this to zero and it will only output the metric, if metric has been set to true. 
  # need to add flag to capture this and exit is the metric has been set to false.
  output_params$features_to_output = 0 #1:5
  
  # print the number of offsets and developments to screen
  output_params$print_dev_offset_sites = FALSE
  
  #ouput offset sites as block colors rather than site_vals
  output_params$output_block_offsets = TRUE
  
  # number of plot sub windows
  output_params$nx = 3 
  output_params$ny = 6
  
  #set of nested lists by scenario and feature (which in this case is 5)
  output_params$site_outcome_plot_lims_set = list(rep(list(c(0, 3e2)), 5))
  output_params$program_outcome_plot_lims_set = list(rep(list(c(0e6, 5e6))), 5)
  output_params$landscape_outcome_plot_lims_set = list(rep(list(c(0, 5e6))))
  
  #set of nested lists by scenario and feature (which in this case is 5)
  output_params$site_impact_plot_lims_set = list(list(c(-1e2, 1e2), c(-1e2, 1e2), c(-1e2, 1e2), c(-5e2, 5e2), c(-5e2, 5e2), c(-5e2, 5e2)))
  output_params$program_impact_plot_lims_set = list(list(c(-5e6, 5e6), c(-2e5, 2e5), c(-2e5, 2e5), c(-2e5, 2e5), c(-5e6, 5e6), c(-5e6, 5e6))) 
  output_params$landscape_impact_plot_lims_set = list(list(c(-1e6, 1e6), c(-1e5, 1e5), c(-1e5, 1e5), c(-1e6, 1e6), c(-1e6, 1e6), c(-5e6, 5e6)))
  
  # write the raw condition value to file if fase, otherwise write the rescaled values as per the colour ramps below.
  output_params$map_vals = TRUE
  
  # Changes these colours 
  black_green.palette <- colorRampPalette(c("black", "green"), space = "rgb")  
  black_blue.palette <- colorRampPalette(c("black", "blue"), space = "rgb")
  
  # set a colour ramp the red is for dev parcels, the orange is for unregulated dev.
  # black_green.palette(128) is for background, black_blue.palette(128) is for offsets
  output_params$col_vec = c(black_green.palette(128), black_blue.palette(128), 'red', 'orange')
  output_params$col_map_vector = c(128, 128, 256, 256, 257) #c(offset_col, offset_bank_col, dev_col, dev_credit_col, unregulated_loss_col)
  
  
  return(output_params)
}

