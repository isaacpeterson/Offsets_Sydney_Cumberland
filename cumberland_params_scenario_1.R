# no developments no offsets

initialise_user_simulation_params <- function(simulation_params, simulated_time_steps, offset_probability_list, development_probability_list, site_characterstics){ 
  
  simulation_params$uncoupled_offset_selection_type = 'pre-determined'  
  simulation_params$development_selection_type = 'stochastic'  

  
  
  uncoupled_offset_control = vector('list', simulated_time_steps)
  uncoupled_offset_control[1:simulated_time_steps] = array(0, simulated_time_steps)
  
  
  simulation_params$uncoupled_offset_control = list(uncoupled_offset_control)
  
  development_control = vector('list', simulated_time_steps)
  development_control[1:simulated_time_steps] = array(0, simulated_time_steps)
  simulation_params$development_control = list(development_control)

  
  return(simulation_params)
  
}


