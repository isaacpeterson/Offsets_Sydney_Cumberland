rm(list = ls())
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(abind)
library(pixmap)
library(offsetsim)
library(gdata)

initialise_ecology_params <- function(){
  
  ecology_params = list()
  ecology_params$region_num = 1
  ecology_params$region_num_x = 1
  ecology_params$region_num_y = 1
  ecology_params$min_eco_val = 0  #minimum allowable ecological value of smallest ecological element (pixel)
  ecology_params$max_eco_val = 100 #maximum "   "     "           "
  
  return(ecology_params)
  
}

build_probability_list <- function(weight_layer, land_parcels){
  
  inds_to_use = !is.na(weight_layer)
  dev_weights = lapply(seq_along(land_parcels), function(i) mean(weight_layer[land_parcels[[i]]]))
  scale_factor = sum(unlist(dev_weights))
  dev_weights = lapply(seq_along(dev_weights), function(i) dev_weights[[i]]/scale_factor)
  
  return(dev_weights)
}


project_data_to_zone_56 <- function(shp_to_project){
  # define projections
  GDA.D <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs ")
  GDA94.56 <- CRS("+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  shp_transform <- spTransform(shp_to_project, GDA94.56) # project to correct CRS
  return(shp_transform)
}
  
load_saved_LGA = TRUE
use_z_layer = TRUE
sample_decline_rate = TRUE

max_eco_val = 100
mean_decline_rate = -0.02
decline_rate_std = 0.005

ecology_params <- initialise_ecology_params()

data_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/cumberland_aligned_tif/')
simulation_inputs_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/simulation_inputs/')
veg_layers = load_rasters(data_folder, c("mac_veg_rst_exprt.tif", "west_veg_rst_exprt.tif", "wilt_veg_rst_exprt.tif"), layer_num = 'all')
cadastre = load_rasters(data_folder, "cad_rst_exprt.tif", layer_num = 'all')
cadastre_msk = load_rasters(data_folder, "cum_sub_rst_exprt.tif", layer_num = 'all')
dev_msk = load_rasters(data_folder, "priority_rast_exprt.tif", layer_num = 'all')

veg_attribute_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/veg_attributes/')
veg_att_files = c('mac_veg_att', 'west_veg_att', 'wilton_veg_att')

veg_num = length(veg_att_files)
veg_attributes = vector('list', veg_num)

for (veg_ind in seq(veg_num)){
  current_veg = veg_att_files[veg_ind]
  veg_attributes[[veg_ind]] = read.xls(paste0(veg_attribute_folder, veg_att_files[veg_ind], '.xls'))
}

names(veg_attributes) = veg_att_files

attribute_vals = list()
attribute_vals$Condition = matrix( ncol=2, byrow=TRUE, 
                      c('', 0,
                        'Thinned', 50,
                        'Scattered Paddock Trees', 20,
                        'Intact', 99,
                        'Urban native / exotic', 0 ,
                        'DNG (to be confirmed)',  0 ,                        
                        'Scattered Trees', 30,
                        'Exotic', 0 ,
                        'Dam', 0 , 
                        'Scattered trees', 30,
                        'DNG', 0, 
                        'Infrastructure', 0,
                        'Scattered Paddock Trees', 20,
                        'Thinned', 50))

attribute_vals$Extent_con = matrix( ncol=2, byrow=TRUE, 
                             c('', 0,
                               'Low', 20,
                               'Moderate', 50,  
                               'High', 90))

attribute_vals$PCT = matrix( ncol=2, byrow=TRUE, 
                             c(0, 0,
                               724, 10,
                               725, 10,
                               806, 20,
                               807, 30,
                               808, 30,
                               830, 30,
                               835, 40,
                               849, 50,
                               850, 50, 
                               877, 60,
                               883, 60, 
                               1067, 70,
                               1081, 70,
                               1083, 70,
                               1181, 70,
                               1292, 70,
                               1395, 70))


veg_vals = vector('list', veg_num)
field_names_to_use = names(attribute_vals)
field_num = length(field_names_to_use)
for (fveg_ind in seq(veg_num)){
  veg_vals[[veg_ind]] = vector('list', field_num)
}

for (veg_ind in seq(veg_num)){
  
  for (field_ind in seq(field_num)){
    field_to_use = match(field_names_to_use[field_ind], names(veg_attributes[[veg_ind]]))
    current_attributes = veg_attributes[[veg_ind]][[field_to_use]]
    unique_attributes = unique(current_attributes)
    current_veg_vals = array(0, length(current_attributes))
    for (attribute_ind in 1:length(unique_attributes)){
      current_attribute_vals = attribute_vals[[field_ind]]
      current_attribute_set = (current_attributes == unique_attributes[attribute_ind])
      match_ind = match(unique_attributes[attribute_ind], current_attribute_vals[, 1])
      
      if (!is.na(match_ind)){
        current_val = as.numeric(current_attribute_vals[, 2][match_ind])
        current_veg_vals[current_attribute_set] = current_val
      }
    }
    veg_vals[[veg_ind]][[field_ind]] = current_veg_vals
  }
}

feature_fields_to_fill = vector('list', field_num)

for (field_ind in 1:field_num){
  feature_fields_to_fill[[field_ind]] = array(0, dim = dim(current_ID_array)[1:2])
}

feature_set = vector('list', veg_num)

for (veg_ind in 1:veg_num){
  
  current_ID_array = raster_to_array(subset(veg_layers, veg_ind))
  array_inds = 1:length(current_ID_array)
  set_to_replace = current_ID_array > 0
  IDs_to_replace = current_ID_array[set_to_replace]
  inds_to_replace = array_inds[set_to_replace]
  
  unique_IDs = unique(as.vector(current_ID_array))
  current_feature_set = feature_fields_to_fill
  
  for (ID_ind in seq_along(unique_IDs)){
    
    current_ID = unique_IDs[ID_ind]
    current_ID_set = (IDs_to_replace == current_ID)
    attribute_ind = which(veg_attributes[[veg_ind]]$OBJECTID == current_ID)
    
    if (any(current_ID_set == TRUE)){
      for (field_ind in seq(field_num)){
        current_inds_to_replace = inds_to_replace[current_ID_set]
        replacement_vals = rep(veg_vals[[veg_ind]][[field_ind]][attribute_ind], length(current_inds_to_replace))
        current_feature_set[[field_ind]][current_inds_to_replace] = replacement_vals
        
      }
    }
    
  }
  feature_set[[veg_ind]] = current_feature_set
}

landscape_ecology = lapply(seq(field_num), function(i) Reduce('+', lapply(seq(veg_num), function(j) feature_set[[j]][[i]])))

parcels = readRDS(paste0(simulation_inputs_folder, 'parcels.rds'))
objects_to_save = list()
objects_to_save$landscape_ecology = landscape_ecology
objects_to_save$cadastre_array = raster_to_array(cadastre)
objects_to_save$cadastre_msk = raster_to_array(cadastre_msk)
# objects_to_save$parcels = define_planning_units(objects_to_save$cadastre_array)
# objects_to_save$parcel_ecology <- split_ecology(objects_to_save$landscape_ecology, objects_to_save$parcels$land_parcels)
objects_to_save$development_mask = raster_to_array(dev_msk) & (raster_to_array(dev_msk) > 0)
objects_to_save$offset_mask = objects_to_save$development_mask

objects_to_save$dev_probability_list <- build_probability_list(objects_to_save$development_mask, land_parcels = parcels$land_parcels)
objects_to_save$offset_probability_list <- build_probability_list(objects_to_save$offset_mask, parcels$land_parcels)

objects_to_save$decline_rates_initial = simulate_decline_rates(parcel_num = length(parcels$land_parcels), 
                                                               sample_decline_rate, 
                                                               rep(list(mean_decline_rate), dim(veg_layers)[3]), 
                                                               rep(list(decline_rate_std), dim(veg_layers)[3]))

save_simulation_inputs(objects_to_save, simulation_inputs_folder)
