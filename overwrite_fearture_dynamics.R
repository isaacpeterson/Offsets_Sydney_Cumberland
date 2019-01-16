management_dynamics_to_use = readRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs/management_dynamics.rds')
feature_dynamics_to_modify = readRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs/feature_dynamics.rds')
offset_block = readRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs/offset_probability_list.rds')
conservation_sites = which(unlist(offset_block)>0)
feature_dynamics_to_modify[conservation_sites] = management_dynamics_to_use[conservation_sites]
#saveRDS('~/offset_data/Sydney_Cumberland_Data/simulation_inputs/feature_dynamics.rds', feature_dynamics_to_modify)