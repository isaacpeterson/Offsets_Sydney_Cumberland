rm(list = ls())
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(abind)
library(pixmap)
library(offsetsim)
library(gdata)



build_feature_layer <- function(feature_type, PCT_set_to_use, current_ID_array, current_data_attributes, condition_class_vals, feature_params, condition_class_bounds){
  
  if (length(PCT_set_to_use) == 0){
    current_feature = vector()
  } else {
    current_condition_class_set = data_attributes[[data_ind]]$Condition[PCT_set_to_use] 
    current_object_ID_set = data_attributes[[data_ind]]$OBJECTID[PCT_set_to_use]
    
    current_feature = matrix(0, dim(current_ID_array)[1], dim(current_ID_array)[2])
    
    for (ID_ind in seq_along(current_object_ID_set)){
      
      current_element_set = which(current_ID_array %in% current_object_ID_set[ID_ind])
      current_condition_class_modes = as.numeric(condition_class_vals[, 2][ match(current_condition_class_set[ID_ind], condition_class_vals[, 1]) ])
      
      if (feature_type == 'Condition_Class'){
        current_element_vals = rep(current_condition_class_modes, length(current_element_set))
      } else if (feature_type == 'Feature_Value'){
        
        current_element_vals = simulate_site_feature_elements(feature_params$site_sample_type,
                                                              current_condition_class_modes,
                                                              condition_class_bounds,
                                                              element_num = length(current_element_set),
                                                              feature_params$initial_site_sd, 
                                                              feature_params$initial_site_mean_sd,
                                                              feature_params$unique_site_vals)
      }
      
      current_feature[current_element_set] = current_element_vals
      
    }
  } 
  return(current_feature)
}







build_probability_list <- function(weight_layer, land_parcels){
  browser()
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
  

source('5_feature_test_params_cumberland.R')

feature_params = initialise_user_feature_params()


overwrite_site_characteristics = FALSE
sample_decline_rate = TRUE


data_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/cumberland_aligned_tif/')
output_data_folder = '/Users/E24661/offset_data/Sydney_Cumberland_Data/prepared_data/'
data_attribute_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/veg_attributes/')

simulation_inputs_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/simulation_inputs/')
priority_data_filenames = c("mac_veg_rst_exprt.tif", "west_veg_rst_exprt.tif", "wilt_veg_rst_exprt.tif")

feature_ID_layers = load_rasters(data_folder, c(priority_data_filenames, "cum_ID_rast1.tif"), features_to_use = 'all')
feature_ID_layers = lapply(seq(dim(feature_ID_layers)[3]), function(i) raster_to_array(subset(feature_ID_layers, i)))

cadastre = load_rasters(data_folder, "cad_rst_exprt.tif", features_to_use = 'all')
cadastre_msk = load_rasters(data_folder, "cum_sub_rst_exprt.tif", features_to_use = 'all')
priority_data_att_files = c('mac_veg_att', 'west_veg_att', 'wilton_veg_att')
dev_msk = raster_to_array(load_rasters(data_folder, "priority_rast_exprt.tif", features_to_use = 'all')) > 0

if (file.exists(paste0(simulation_inputs_folder, 'site_characteristics.rds')) & (overwrite_site_characteristics == FALSE)){
  site_characteristics = readRDS(paste0(simulation_inputs_folder, 'site_characteristics.rds'))
} else {
  cadastre_array = raster_to_array(cadastre)
  site_characteristics = define_planning_units(cadastre_array)
  saveRDS(object = site_characteristics, file = paste0(simulation_inputs_folder, 'site_characteristics.rds'))
}


dev_probability_list = build_probability_list(dev_msk, site_characteristics$land_parcels)
offset_probability_list = build_probability_list(!dev_msk, site_characteristics$land_parcels)



data_num = length(priority_data_att_files)
priority_data_attributes = vector('list', data_num)

for (data_ind in seq(data_num)){
  current_veg = priority_data_att_files[data_ind]
  priority_data_attributes[[data_ind]] = read.xls(paste0(data_attribute_folder, priority_data_att_files[data_ind], '.xls'))
}


priority_condition_att = lapply(seq_along(priority_data_attributes), function(i) priority_data_attributes[[i]][match(c('PCT', 'Condition', 'Shape_Area'), names(priority_data_attributes[[i]]))])
priority_condition_set = do.call("rbind", priority_condition_att)

cumberland_att = read.xls(paste0(data_attribute_folder, 'cum_veg_att.xls'))

area_cuts_to_use = c(seq(0, 1e5, by = 1e4), Inf)
labels_to_use = seq(length(area_cuts_to_use) - 1)

PCT_to_use = 849 #unique(priority_condition_set$PCT)
feature_num = 5
min_data_count = 50


condition_class_vals = matrix( ncol=2, byrow=TRUE, 
                               c('Intact', 4,
                                 'Thinned', 3,
                                 ' ', 0, 
                                 'Scattered Paddock Trees', 1,
                                 'DNG (to be confirmed)', 0, 
                                 'DNG', 0, 
                                 'Urban native / exotic', 0, 
                                 'Scattered Trees', 2,
                                 'Exotic', 0, 
                                 'Dam', 0, 
                                 'Scattered trees', 2, 
                                 'Infrastructure', 0))


priority_cuts_by_PCT = vector('list', length(PCT_to_use))

for (PCT_ind in seq_along(PCT_to_use)){
  current_PCT_set = (priority_condition_set$PCT == PCT_to_use[PCT_ind])
  
  current_cuts = cut(priority_condition_set$Shape_Area[current_PCT_set], area_cuts_to_use, labels = labels_to_use)
  
  current_condition_set = priority_condition_set$Condition[current_PCT_set]
  current_cut_set = vector('list', length(labels_to_use))
  for (cut_ind in seq(length(labels_to_use))){
    current_cut_set[[cut_ind]] = current_condition_set[current_cuts == cut_ind]
  }
  priority_cuts_by_PCT[[PCT_ind]] = current_cut_set
}

full_cuts = cut(priority_condition_set$Shape_Area, area_cuts_to_use, labels = labels_to_use)

full_priority_cut_stats = vector('list', length(labels_to_use))

for (cut_ind in seq(length(labels_to_use))){
  full_priority_cut_stats[[cut_ind]] = as.data.frame(table(priority_condition_set$Condition[full_cuts == cut_ind]))
}

cumberland_cuts = cut(cumberland_att$Shape_Area, area_cuts_to_use, labels = labels_to_use)
cumberland_conditions = vector(mode="character", length(cumberland_att$Shape_Area))



for (site_ind in seq(length(cumberland_att$Shape_Area))){
  
  if (cumberland_att$PCT[site_ind] %in% PCT_to_use){
    current_priority_stats = as.data.frame(table(priority_cuts_by_PCT[[match(cumberland_att$PCT[site_ind], PCT_to_use)]][[cumberland_cuts[site_ind]]]))
    if (sum(current_priority_stats$Freq) < min_data_count){
      current_priority_stats = as.data.frame(table(unlist(priority_cuts_by_PCT[[match(cumberland_att$PCT[site_ind], PCT_to_use)]])))
    }
    if (sum(current_priority_stats$Freq) < min_data_count){
      current_priority_stats = full_priority_cut_stats[[cumberland_cuts[site_ind]]]
    }
  } else {
    current_priority_stats = full_priority_cut_stats[[cumberland_cuts[site_ind]]]
  }
  
  current_cond_inds = sample(x = seq_along(current_priority_stats$Freq), size = 1, prob = current_priority_stats$Freq/sum(current_priority_stats$Freq), replace = TRUE)
  cumberland_conditions[site_ind] = as.character(current_priority_stats$Var1[as.numeric(current_cond_inds)])
}

cumberland_att$Condition = cumberland_conditions


data_attributes = append(priority_data_attributes, list(cumberland_att))

names(data_attributes) = c(priority_data_att_files, 'cum_veg_att')

condition_class_set = lapply(seq_along(data_attributes), function(i) vector('list', length(PCT_to_use)))

for (data_ind in seq_along(data_attributes)){
  
  for (PCT_ind in seq_along(PCT_to_use)){
    
    PCT_set_to_use = which(data_attributes[[data_ind]]$PCT == PCT_to_use[PCT_ind])
    current_feature = build_feature_layer(feature_type = 'Condition_Class', 
                                          PCT_set_to_use, 
                                          current_ID_array = feature_ID_layers[[data_ind]], 
                                          current_data_attributes = data_attributes[[data_ind]], 
                                          condition_class_vals, 
                                          feature_params, 
                                          condition_class_bounds = vector())
    
    if (names(data_attributes)[data_ind] ==  'cum_veg_att'){
      current_feature = current_feature*(dev_msk == 0)
    } else {
      current_feature = current_feature*dev_msk
    }
    condition_class_set[[data_ind]][[PCT_ind]] = current_feature

  }
}



merged_condition_classes = vector('list', length(PCT_to_use))

for (PCT_ind in seq_along(PCT_to_use)){
  inds_to_use = which(unlist(lapply(seq_along(condition_class_set), function(i) length(condition_class_set[[i]][[PCT_ind]]) > 0)))
  merged_condition_classes[[PCT_ind]] = Reduce('+', lapply(inds_to_use, function(i) condition_class_set[[i]][[PCT_ind]]))
}


for (PCT_ind in seq_along(PCT_to_use)){
  current_feature_raster = raster(merged_condition_classes[[PCT_ind]])
  current_file_name = paste0(output_data_folder, 'PCT_', PCT_to_use[PCT_ind], '_condition_class_layer.tif')
  writeRaster(current_feature_raster, current_file_name, overwrite = TRUE)
  print(current_file_name) 
}

mode_selection_type = 'feature_layer'

for (PCT_ind in seq_along(PCT_to_use)){
  
  current_site_condition_class_modes = vector('list', length(site_characteristics$land_parcels))
  for (site_ind in seq_along(site_characteristics$land_parcels)){
    
    current_vals = merged_condition_classes[[PCT_ind]][site_characteristics$land_parcels[[site_ind]]]
    
    if (mode_selection_type == 'feature_layer'){
      current_site_condition_class_modes[[site_ind]] = rep(list(current_vals), feature_num)
    } else {
      current_vals = current_vals[current_vals > 0]
      if (length(current_vals) > 0){
        if (mode_selection_type == 'most_common_modes'){
          current_site_condition_class_modes[[site_ind]] = rep(list(as.numeric(tail(names(sort(table(current_vals), decreasing = TRUE )[1])))), feature_num)
        } else if (mode_selection_type == 'unique_modes'){
          current_site_condition_class_modes[[site_ind]] = rep(list(unique(as.vector(current_vals))), feature_num)
        }
      } else {
        current_site_condition_class_modes[[site_ind]] = rep(list(0), feature_num)
      }
    }
  }
  
  saveRDS(object = current_site_condition_class_modes, paste0(output_data_folder, 'PCT_', PCT_to_use[PCT_ind], '_condition_class_object.rds'))
  
}


condition_vals_set = lapply(seq_along(data_attributes), 
                            function(i) lapply(seq_along(PCT_to_use), 
                                               function(j) vector('list', feature_params$simulated_feature_num)))

for (data_ind in seq_along(data_attributes)){
  
  for (PCT_ind in seq_along(PCT_to_use)){
    PCT_set_to_use = which(data_attributes[[data_ind]]$PCT == PCT_to_use[PCT_ind])
    
    for (feature_ind in seq(feature_params$simulated_feature_num)){
      
      current_feature = build_feature_layer(feature_type = 'Feature_Value', 
                                            PCT_set_to_use, 
                                            current_ID_array = feature_ID_layers[[data_ind]], 
                                            current_data_attributes = data_attributes[[data_ind]], 
                                            condition_class_vals, 
                                            feature_params, 
                                            condition_class_bounds = feature_params$initial_condition_class_bounds[[feature_ind]])
      
      if (names(data_attributes)[data_ind] ==  'cum_veg_att'){
        current_feature = current_feature*(dev_msk == 0)
      } else {
        current_feature = current_feature*dev_msk
      }
      
      condition_vals_set[[data_ind]][[PCT_ind]][[feature_ind]] = current_feature
    }
    
  }
}




merged_condition_vals = lapply(seq_along(PCT_to_use), function(i) vector('list', feature_num))
                               
for (PCT_ind in seq_along(PCT_to_use)){
  for (feature_ind in seq(feature_num)){
    inds_to_use = which(unlist(lapply(seq_along(condition_class_set), function(i) length(condition_vals_set[[i]][[PCT_ind]][[feature_ind]]) > 0)))
    merged_condition_vals[[PCT_ind]][[feature_ind]] = Reduce('+', lapply(inds_to_use, function(i) condition_vals_set[[i]][[PCT_ind]][[feature_ind]]))
  }
}



for (PCT_ind in seq_along(PCT_to_use)){
  for (feature_ind in seq(feature_num)){
    current_feature_raster = raster(merged_condition_vals[[PCT_ind]][[feature_ind]])
    current_file_name = paste0(output_data_folder, 'PCT_', PCT_to_use[PCT_ind], '_feature_', feature_ind, '.tif')
    writeRaster(current_feature_raster, current_file_name, overwrite = TRUE)
    print(current_file_name) 
  }
}








