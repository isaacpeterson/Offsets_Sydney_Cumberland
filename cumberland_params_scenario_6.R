# Offsets implemented incrementally over simulation in strategic area, high
# intensity management

initialise_user_simulation_params <- function(simulation_params, simulated_time_steps, offset_probability_list, development_probability_list, site_characterstics){ 
  
  simulation_params$uncoupled_offset_selection_type = 'stochastic'  
  simulation_params$development_selection_type = 'stochastic' 
  
  offset_sites_to_use = site_characteristics$site_IDs[which(unlist(offset_probability_list) > 0)]
  
  simulation_params$uncoupled_offset_control = list(build_stochastic_intervention(simulated_time_steps, 
                                                                                  intervention_start = 1, 
                                                                                  intervention_end = simulated_time_steps, 
                                                                                  intervention_num = length(offset_sites_to_use), 
                                                                                  sd = 1))
  
  dev_num = length(which(unlist(development_probability_list) > 0))
  
  simulation_params$development_control = list(build_stochastic_intervention(simulated_time_steps, 
                                                                             intervention_start = 1, 
                                                                             intervention_end = simulated_time_steps, 
                                                                             intervention_num = dev_num, 
                                                                             sd = 1))
  
  return(simulation_params)
  
}

