initialise_user_global_params <- function(){
  
  global_params = list()
  
  global_params$simulation_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/')
  
  global_params$use_simulated_data = FALSE
  
  global_params$number_of_cores = 1
  
  # The number of realizations to run
  global_params$realisation_num = 1
  
  # Makes a single pdf at the end of the simulation showing the locatons of all offsets
  global_params$write_offset_layer = TRUE
  
  # Create an animation of the outputs
  global_params$write_movie = TRUE
  
  return(global_params)
}


initialise_user_simulation_params <- function(){ 
  
  simulation_params = list()
  
  # what subset of features to use in the simulation
  simulation_params$features_to_use_in_simulation = 1:3
  
  # The total number of layers to use in the offset calcuation (iterating from the start)
  simulation_params$features_to_use_in_offset_calc = 1:3
  
  simulation_params$features_to_use_in_offset_intervention = 1:3

  # How long to run the simulaton in years
  simulation_params$time_steps = 50
  simulation_params$intervention_num = 1000
  
  simulation_params$intervention_vec = generate_stochastic_intervention_vec(time_steps = simulation_params$time_steps, 
                                                                            intervention_start = 1, 
                                                                            intervention_end = simulation_params$time_steps, 
                                                                            simulation_params$intervention_num, 
                                                                            sd = 1)
  
  # The maxoimum number of parcels can be selected to offset a single development
  
  simulation_params$max_offset_parcel_num = 20
  
  # Stops the offset from delivering any further gains once it has acheived the gains required
  simulation_params$limit_offset_restoration = TRUE
  
  # The probability per parcel of it being stochasticly cleared, every parcel gets set to this number - set to zero to turn off
  simulation_params$unregulated_loss_prob = 0
  
  # Exclude parcels with less than this number of pixels.
  simulation_params$site_screen_size = 2
  
  # The mean and the standard deviation of a normal distribution from which to sample the restoration parameters from
  simulation_params$restoration_rate = 0.02
  
  simulation_params$restoration_rate_std = 0.005
  #   c('net_gains', 'restoration_gains', 'avoided_condition_decline', 'avoided_loss',
  #     'protected_condition', 'current_condition', 'restored_condition')
  
  simulation_params$offset_action_params = list(c('net_gains', 'restore'))
  
  #use a specified offset metric in the site match calculation
  simulation_params$use_specified_offset_metric = TRUE
  
  # define the offset metric function
  simulation_params$offset_metric_type = 'euclidian_norm'
  
  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
  simulation_params$dev_calc_type = 'future_condition'    #'future_condition', 'current_condition' 
  
  # Track accumulated credit from previous exchanges (eithger in current or
  # previous time step) and use them to allow developments to proceed if the
  # credit is large enough. FALSE means ignore any exces credit from offset exchanges
  simulation_params$allow_developments_from_credit = TRUE
  
  # How the development parcels are selected options are 'random' or
  # 'weighted'. Note tha weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # intialise_routines.R  (or put the files in simulation_inputs)
  simulation_params$development_selection_type = 'random'  
  
  # Whether to use banking. FALSE - means perform offsets simultaneously with development, TRUE -
  # means perform offset banking prior to development according to offset bank
  # parameters
  simulation_params$use_offset_bank = FALSE

  # The time horizon in which the offset gains need to equal the devlopment impact
  simulation_params$offset_time_horizon = 30
  
  # Include future stochastic developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  simulation_params$include_unregulated_loss_in_offset_calc = FALSE
  
  # Include stochastic clearing in the calculating the contribution of avoided
  # losses to the impact of the development. 
  # simulation_params$include_unregulated_loss_in_dev_calc = simulation_params$include_unregulated_loss_in_offset_calc
  
  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  simulation_params$include_potential_developments_in_offset_calc = FALSE

  simulation_params$dev_counterfactual_adjustment = 'as_offset'
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  simulation_params$offset_multiplier = 1
  
  
  return(simulation_params)
  
}



initialise_user_output_params <- function(){
  output_params = list()
  output_params$output_plot_folder = vector()
  output_params$plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
  output_params$output_plot = TRUE # set to plot through 'features', 'scenarios' or 'site_sets'
  output_params$realisation_num = 'all' # 'all' or number to plot
  output_params$write_pdf = FALSE
  output_params$sets_to_plot = 10 # example site to plot
  output_params$scenario_vec = 'all' #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  output_params$site_impact_col_vec = c('darkgreen', 'red', 'black')
  output_params$program_col_vec = c('darkgreen', 'red', 'black') 
  output_params$cfac_col = 'blue' 
  output_params$landscape_col = 'black'
  output_params$lwd_vec = c(3, 0.5)
  output_params$site_impact_lwd = 0.5
  output_params$site_outcome_lwd_vec = c(0.5)
  output_params$program_lwd_vec = c(3, 0.5)
  output_params$program_outcome_lwd_vec = c(3, 0.5)
  output_params$landscape_lwd_vec  = c(3)
  output_params$landscape_outcome_lwd_vec = c(3)
  output_params$string_width = 3 # how many digits are used to store scenario index and realisation index
  output_params$nx = 3 
  output_params$ny = 4
  
  output_params$site_outcome_plot_lims_set = rep(list(c(0, 3e4)), 10)
  output_params$program_outcome_plot_lims_set = rep(list(c(0e6, 1e7)), 10)
  output_params$landscape_outcome_plot_lims_set = rep(list(c(0, 2e7)), 10)
  
  output_params$site_impact_plot_lims_set = rep(list(c(-5e2, 5e2)), 10)
  output_params$program_impact_plot_lims_set = rep(list(c(-1e4, 1e4)), 10) 
  output_params$landscape_impact_plot_lims_set = rep(list(c(-5e5, 0)), 10)
  
  
  
  return(output_params)
}

