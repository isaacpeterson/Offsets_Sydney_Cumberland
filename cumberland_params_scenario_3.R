# offsets secured at yr 1 with low intensity

initialise_user_simulation_params <- function(simulation_params, simulated_time_steps){ 
  
  simulation_params$use_offset_bank = TRUE
  
  # 'pre-determined' - list site id's specifically 
  simulation_params$banked_offset_selection_type = 'pre_determined'  
  banked_offset_control = vector('list', simulated_time_steps)
  
  banked_offset_control[1:simulated_time_steps] = array(0, simulated_time_steps)
  
  #these are the actual site ids specified in the raster layer
  offset_probability_list = readRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs_jan_17/offset_probability_list.rds')
  site_characteristics = readRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs_jan_17/site_characteristics.rds')
  banked_offset_sites_to_use = site_characteristics$site_IDs[which(unlist(offset_probability_list) > 0)]

  
  # [[1]] indicates all offsets to take place on year 1
  banked_offset_control[[1]] = banked_offset_sites_to_use
  
  simulation_params$banked_offset_control = list(banked_offset_control)
  simulation_params$offset_bank_type = 'credit'    
  
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

