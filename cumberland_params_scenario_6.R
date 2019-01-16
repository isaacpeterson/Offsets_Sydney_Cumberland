# offsets incrementally occur over simulation in strategic area, high intensity management

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
  global_params$time_steps = 37
  global_params$features_to_use_in_simulation = 1:5
  
  global_params$store_zeros_as_sparse = TRUE
  
  # set to FALSE if running into memory troubles when collating
  global_params$collate_with_parallel_cores = TRUE
  # The number of cores to run on.
  global_params$number_of_cores = 'all'
  
  # The number of realizations to run
  global_params$realisation_num = 2
  
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
  
  return(global_params)
}




initialise_user_simulation_params <- function(simulated_time_steps){ 
  
  simulation_params = list()
  
  # The probability per parcel of it being unregulatedly cleared, every parcel
  # gets set to this number - set to zero to turn off. Be careful as this is
  # dependant on the total number of parcels.
  simulation_params$unregulated_loss_prob = 0.001
  
  # The total number of layers to use in the offset calcuation (iterating from the start)
  # Need to keep these as is to use veg integrity score
  simulation_params$features_to_use_in_offset_calc = 1:5
  
  # Speifies what is affected by the offset intervention
  # Need to keep these as is to use veg integrity score
  simulation_params$features_to_use_in_offset_intervention = 1:5
  
  # means using the BAM score via the veg integrity. If this is TRUE thenm the
  # offset metric needs to be defined via a function called
  # user_transform_function, which is defined below.
  simulation_params$use_offset_metric = TRUE
  
  # These are the benchmark scores for tree/grass richness/cover etc that are
  # used in the calculating the veg integrity score
  simulation_params$transform_params = c(5, 12, 15, 22, 45)
  
  # The maximum number of parcels can be selected to offset a single development
  
  simulation_params$max_offset_parcel_num = 10
  
  # Stops the offset from delivering any further gains once it has acheived the gains required
  # only applied to management stopping. Still gets avoided lose gains
  simulation_params$limit_offset_restoration = TRUE
  
  # Exclude parcels with less than this number of pixels.
  simulation_params$min_site_screen_size = 5
  
  # Removing the very largest parcels in the top 0.1% of the size distribution.
  simulation_params$max_site_screen_size_quantile = 0.999
  
  
  #   c('net_gains', 'restoration_gains', 'avoided_condition_decline', 'avoided_loss',
  #     'protected_condition', 'current_condition', 'restored_condition')
  
  simulation_params$offset_action_params = list(c('net_gains', 'restore'))
  
  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
  simulation_params$dev_calc_type = 'future_condition'    #'future_condition', 'current_condition' 
  
  # Track accumulated credit from previous exchanges (eithger in current or
  # previous time step) and use them to allow developments to proceed if the
  # credit is large enough. FALSE means ignore any exces credit from offset exchanges
  simulation_params$allow_developments_from_credit = TRUE
  
  # The time horizon in which the offset gains need to equal the devlopment impact
  simulation_params$offset_time_horizon = 30
  
  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  simulation_params$include_potential_developments_in_offset_calc = list(FALSE)
  
  # Include future unregulated developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  simulation_params$include_unregulated_loss_in_offset_calc = list(FALSE)
  
  # Include unregulated clearing in the calculating the contribution of avoided
  # losses to the impact of the development. 
  simulation_params$include_unregulated_loss_in_dev_calc = simulation_params$include_unregulated_loss_in_offset_calc
  
  simulation_params$dev_counterfactual_adjustment = 'as_offset'
  
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  simulation_params$offset_multiplier = 1
  
  #set to greater than zero to allow developments without offsets
  simulation_params$initial_credit = 1e10
  
  # set to false to stop initial credit being transformed - this has a big impact when using the BAM metric which 
  # transforms large values to ceiling defined by 100.68
  simulation_params$transform_initial_credit = FALSE
  
  simulation_params$use_offset_bank = TRUE
  simulation_params$offset_bank_type = 'credit' 
  simulation_params$banked_offset_selection_type = 'stochastic'  
  
  simulation_params$banked_offset_control = list(build_stochastic_intervention(simulated_time_steps, 
                                                                               intervention_start = 1, 
                                                                               intervention_end = simulated_time_steps, 
                                                                               intervention_num = 50e3, 
                                                                               sd = 1))
  
  # How the development parcels are selected options are 'stochastic',
  # 'weighted', or 'pre-determined' where a predetermined development vector is passed in as a list. 
  # Note that weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # intialise_routines.R  (or put the files in simulation_inputs)
  simulation_params$development_selection_type = 'stochastic'  
  
  simulation_params$development_control = list(build_stochastic_intervention(simulated_time_steps, 
                                                                             intervention_start = 1, 
                                                                             intervention_end = simulated_time_steps, 
                                                                             intervention_num = 3789, 
                                                                             sd = 1))
  
  #ignore offset sites with zero value
  simulation_params$screen_offset_zeros = TRUE
  
  # ignore development sites with zero value
  simulation_params$screen_dev_zeros = FALSE
  
  
  return(simulation_params)
  
}


collate_dynamics <- function(full_dynamics_set, datasheets_to_use, set_index_to_use){
  
  current_dynamics_set = lapply(seq_along(datasheets_to_use), 
                                function(i) lapply(datasheets_to_use[[i]], 
                                                   function(j) full_dynamics_set[[j]][[set_index_to_use]] ))
  
  current_dynamics_set = lapply(seq_along(current_dynamics_set),  
                                function(i) lapply(seq_along(current_dynamics_set[[i]]), 
                                                   function(j) setNames(current_dynamics_set[[i]][[j]], c('lower_bound', 'best_estimate', 'upper_bound'))))
  return(current_dynamics_set)
}


user_transform_function <- function(pool_vals, transform_params){
  scaled_scores <- lapply(seq_along(pool_vals), function(i) transform_params[i]/sum(transform_params)*100.68*(1 - exp(-5*( pool_vals[[i]]/transform_params[i] )^2.5) ))
  BAM_score <- sqrt(Reduce('+', scaled_scores[1:3]) * Reduce('+', scaled_scores[4:5]))
  return(BAM_score)
}



initialise_user_feature_params <- function(current_author_splines){
  
  feature_params = list()
  feature_params$scale_features = FALSE
  # how the feature dynamics are determined
  feature_params$background_dynamics_type = 'site_scale'
  feature_params$management_dynamics_type = 'site_scale'
  
  feature_params$site_sample_type = 'trunc_norm'
  feature_params$initial_site_sd = 0.05
  
  feature_params$initial_site_mean_sd = 0.2
  feature_params$dynamics_sample_type = 'by_initial_value' #'by_initial_value' 
  # Sample the restoration rates from a uniform distribution to they vary per parcel and per feature
  feature_params$management_dynamics_sample_type = 'by_initial_value'
  
  feature_params$project_by_mean = TRUE
  
  feature_params$update_management_dynamics_by_differential = TRUE
  feature_params$update_background_dynamics_by_differential = TRUE
  
  feature_params$perform_management_dynamics_time_shift = FALSE
  feature_params$perform_background_dynamics_time_shift = FALSE
  
  feature_params$sample_management_dynamics = TRUE
  feature_params$sample_background_dynamics = TRUE
  
  #how many feature layers to generate
  feature_params$simulated_feature_num = 5
  
  
  # The larger this value is the more extrapolating with the splines
  
  
  #the time over which the experts defined the curves
  
  feature_params$simulated_time_vec = 0:80
  #   full_dynamics_set = lapply(seq_along(current_author_splines),  
  #                              function(i) lapply(seq_along(current_author_splines[[i]]), 
  #                                                 function(j) lapply(seq_along(current_author_splines[[i]][[j]]), 
  #                                                                    function(k) predict(current_author_splines[[i]][[j]][[k]], 
  #                                                                                        feature_params$simulated_time_vec)$y)))
  
  # full_dynamics_set is a list of vectors for each of the expert curves
  # map splines to simulated_time_vec, first loop  
  
  # k loop: upper bound best estimtate lower bound
  # j loop: veg condition type (L, M1, M2, H)
  # i look: lifeform: grass, trees etc
  
  
  full_dynamics_set = lapply(seq_along(current_author_splines),  
                             function(i) lapply(seq_along(current_author_splines[[i]]), 
                                                function(j) lapply(seq_along(current_author_splines[[i]][[j]]), 
                                                                   function(k) current_author_splines[[i]][[j]][[k]](feature_params$simulated_time_vec))))
  
  # This is just reordering the best upper and lower bound
  full_dynamics_set = lapply(seq_along(full_dynamics_set),  
                             function(i) lapply(seq_along(full_dynamics_set[[i]]), 
                                                function(j) full_dynamics_set[[i]][[j]][c(1, 3, 2)])) 
  
  # This is to set any value below zero back to zero (can get negative numbers from spline extrapolation)
  full_dynamics_set = lapply(seq_along(full_dynamics_set),  
                             function(i) lapply(seq_along(full_dynamics_set[[i]]), 
                                                function(j) lapply(seq_along(full_dynamics_set[[i]][[j]]), 
                                                                   function(k) full_dynamics_set[[i]][[j]][[k]]*(full_dynamics_set[[i]][[j]][[k]] >= 0)))) 
  
  background_datasheets = list(1:4, 5:8, 9:12,  13:16,  17:20) 
  management_datasheets = list(1:3, 5:7, 9:11, 13:15, 17:19) 
  unmanaged_datasheets = list(4, 8, 12, 16, 20)
  
  background_dynamics_set = collate_dynamics(full_dynamics_set, background_datasheets, set_index_to_use = 1)
  
  # LOW INTENSITY: set_index_to_use = 2 , HIGH INTENSITY set_index_to_use = 3
  management_dynamics_set = collate_dynamics(full_dynamics_set, management_datasheets, set_index_to_use = 3)
  
  unmanaged_set = collate_dynamics(full_dynamics_set, unmanaged_datasheets, set_index_to_use = 1)
  
  management_dynamics_set = lapply(seq_along(management_dynamics_set), function(i) append(management_dynamics_set[[i]], unmanaged_set[[i]]))
  
  #management_dynamics_set = lapply(seq_along(management_dynamics_set), function(i) append(management_dynamics_set[[i]], management_dynamics_set[[i]][3]))
  
  #high_intensity_management_set = collate_dynamics(full_dynamics_set, management_datasheets, set_index_to_use = 3)
  
  feature_params$initial_condition_class_bounds = lapply(seq_along(background_dynamics_set), 
                                                         function(i) lapply(seq_along(background_dynamics_set[[i]]), 
                                                                            function(j) c(max(background_dynamics_set[[i]][[j]]$lower_bound[1], management_dynamics_set[[i]][[j]]$lower_bound[1]), 
                                                                                          mean(background_dynamics_set[[i]][[j]]$best_estimate[1], management_dynamics_set[[i]][[j]]$best_estimate[1]), 
                                                                                          min(background_dynamics_set[[i]][[j]]$upper_bound[1], management_dynamics_set[[i]][[j]]$upper_bound[1]))))
  
  feature_params$condition_class_bounds = lapply(seq_along(background_dynamics_set), 
                                                 function(i) lapply(seq_along(background_dynamics_set[[i]]), 
                                                                    function(j) c(min(c(background_dynamics_set[[i]][[j]]$lower_bound, management_dynamics_set[[i]][[j]]$lower_bound)), 
                                                                                  mean(c(background_dynamics_set[[i]][[j]]$best_estimate, management_dynamics_set[[i]][[j]]$best_estimate)), 
                                                                                  max(c(background_dynamics_set[[i]][[j]]$upper_bound, management_dynamics_set[[i]][[j]]$upper_bound)) )))
  
  
  #   setup_sub_plots(nx = 4, ny = 5, x_space = 2, y_space = 2)
  #   
  #   for (feature_ind in 1:5){
  #     for (mode_ind in 1:4){
  #       plot(background_dynamics_set[[feature_ind]][[mode_ind]]$lower_bound, type = 'l', col = 'blue', ylim = c(0, 40))
  #       lines(background_dynamics_set[[feature_ind]][[mode_ind]]$best_estimate, type = 'l', col = 'black')
  #       lines(background_dynamics_set[[feature_ind]][[mode_ind]]$upper_bound, type = 'l', col = 'red')
  #       
  #       
  #     }
  #   }
  
  feature_params$management_condition_class_bounds = feature_params$condition_class_bounds
  
  feature_params$background_dynamics_bounds = background_dynamics_set
  feature_params$management_dynamics_bounds = management_dynamics_set
  
  return(feature_params)
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
  
  output_params$plot_type = 'impacts' # can be 'outcomes'  or 'impacts'
  
  # use 'all' for all or therwise the numern eg 6 means the first 6 realiztaions.
  
  output_params$realisation_num = 'all' # 'all' or number to plot
  
  # the dev-offset to plot for the site level results.
  output_params$sets_to_plot = 1
  output_params$plot_site = TRUE
  output_params$plot_program = TRUE
  output_params$plot_landscape = TRUE
  
  # if this is false won't generate plots of maps for the offset metric
  output_params$plot_offset_metric = TRUE
  
  output_params$scenario_vec = 'all' #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  
  output_params$write_pdf = FALSE
  output_params$output_type = 'png' # set to 'raster', 'png', 'plot', or 'csv'
  
  # was originally done 
  output_params$plot_subset_type = 'all' #c('offset_action_type') # 'offset_calc_type', 'offset_action_type', offset_time_horizon'
  output_params$plot_subset_param = 'all' #c('maintain') # 'net_gains', 'restore', 15
  
  # Set this to zero and it will only output the metric, if metric has been set to true. 
  # need to add flag to capture this and exit is the metric has been set to false.
  output_params$features_to_output = 0 #1:5
  
  # print the number of offsets and developments to screen
  output_params$print_dev_offset_sites = FALSE
  
  #ouput offset sites as block colors rather than site_vals
  output_params$output_block_offsets = FALSE
  
  # number of plot sub windows
  output_params$nx = 3 
  output_params$ny = 6
  
  #set of nested lists by scenario and feature (which in this case is 5)
  output_params$site_outcome_plot_lims_set = list(rep(list(c(0, 1e2)), 5))
  output_params$program_outcome_plot_lims_set = list(rep(list(c(0e6, 1e5))), 5)
  output_params$landscape_outcome_plot_lims_set = list(rep(list(c(0, 2e5))))
  
  #set of nested lists by scenario and feature (which in this case is 5)
  output_params$site_impact_plot_lims_set = list(list(c(-1e2, 1e2), c(-1e2, 1e2), c(-1e2, 1e2), c(-5e2, 5e2), c(-5e2, 5e2), c(-5e2, 5e2)))
  output_params$program_impact_plot_lims_set = list(list(c(-1e5, 1e5), c(-2e5, 2e5), c(-2e5, 2e5), c(-2e5, 2e5), c(-2e5, 2e5), c(-2e5, 2e5))) 
  output_params$landscape_impact_plot_lims_set = list(list(c(-1e5, 1e5), c(-1e5, 1e5), c(-1e5, 1e5), c(-1e6, 1e6), c(-1e6, 1e6), c(-1e6, 1e6)))
  
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

