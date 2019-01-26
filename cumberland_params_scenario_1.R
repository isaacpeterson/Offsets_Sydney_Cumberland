# no developments no offsets

initialise_user_simulation_params <- function(simulation_params, simulated_time_steps){ 
  
  simulation_params$use_offset_bank = FALSE
  simulation_params$banked_offset_selection_type = 'pre_determined'  
  banked_offset_control = vector('list', simulated_time_steps)
  banked_offset_control[1:simulated_time_steps] = array(0, simulated_time_steps)
  simulation_params$banked_offset_control = list(banked_offset_control)
  simulation_params$offset_bank_type = 'credit'     

  # How the development parcels are selected options are 'stochastic',
  # 'weighted', or 'pre-determined' where a predetermined development vector is passed in as a list. 
  # Note that weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # intialise_routines.R  (or put the files in simulation_inputs)
  simulation_params$development_selection_type = 'pre_determined'  
  
  development_control = vector('list', simulated_time_steps)
  development_control[1:simulated_time_steps] = array(0, simulated_time_steps)
  simulation_params$development_control = list(development_control)

  
  return(simulation_params)
  
}


