# offsets occur at simulation initialisation, high intensity management, with only a randomly selected half of the offsets secured

initialise_user_simulation_params <- function(simulation_params, simulated_time_steps, offset_probability_list, development_probability_list, site_characterstics){ 
  
  simulation_params$uncoupled_offset_selection_type = 'pre_determined'  
  simulation_params$development_selection_type = 'stochastic' 
  
  offset_sites_to_use = site_characteristics$site_IDs[which(unlist(offset_probability_list) > 0)]

  # set this to be only half the number of offsets
  number_to_sample = round(length(offset_sites_to_use)*0.5)
  offset_sites_to_use = sample(offset_sites_to_use, number_to_sample, replace = FALSE )

  
  uncoupled_offset_control = vector('list', simulated_time_steps)
  uncoupled_offset_control[1:simulated_time_steps] = array(0, simulated_time_steps)
  
  uncoupled_offset_control[[1]] = offset_sites_to_use
  simulation_params$uncoupled_offset_control = list(uncoupled_offset_control)

  dev_num = length(which(unlist(development_probability_list) > 0))
  
  simulation_params$development_control = list(build_stochastic_intervention(simulated_time_steps, 
                                                                             intervention_start = 1, 
                                                                             intervention_end = simulated_time_steps, 
                                                                             intervention_num = dev_num, 
                                                                             sd = 1))
  
  return(simulation_params)
  
}
