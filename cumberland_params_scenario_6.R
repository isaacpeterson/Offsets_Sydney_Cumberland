# offsets incrementally occur over simulation in strategic area, high intensity management

initialise_user_simulation_params <- function(simulation_params, simulated_time_steps){ 
  
  simulation_params$use_offset_bank = TRUE
  simulation_params$offset_bank_type = 'credit' 
  simulation_params$banked_offset_selection_type = 'stochastic'  
  
  #these are the actual site ids specified in the raster layer
  offset_probability_list = readRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs_jan_17/offset_probability_list.rds')
  site_characteristics = readRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs_jan_17/site_characteristics.rds')
  banked_offset_sites_to_use = site_characteristics$site_IDs[which(unlist(offset_probability_list) > 0)]
  
  simulation_params$banked_offset_control = list(build_stochastic_intervention(simulated_time_steps, 
                                                                               intervention_start = 1, 
                                                                               intervention_end = simulated_time_steps, 
                                                                               intervention_num = length(banked_offset_sites_to_use), 
                                                                               sd = 1))
  
  # How the development parcels are selected options are 'stochastic',
  # 'weighted', or 'pre-determined' where a predetermined development vector is passed in as a list. 
  # Note that weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # intialise_routines.R  (or put the files in simulation_inputs)
  simulation_params$development_selection_type = 'stochastic'  
  
  development_probability_list = readRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs_jan_17/dev_probability_list.rds')
  dev_num = length(which(unlist(development_probability_list) > 0))
  
  simulation_params$development_control = list(build_stochastic_intervention(simulated_time_steps, 
                                                                             intervention_start = 1, 
                                                                             intervention_end = simulated_time_steps, 
                                                                             intervention_num = dev_num, 
                                                                             sd = 1))
  
  return(simulation_params)
  
}

