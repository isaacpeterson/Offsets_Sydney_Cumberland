library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(abind)
library(pixmap)
library(gdata)
library(offsetsim)

# This function says: I have a data attributes table, take the data attributes table and info on
# current_ID_array array of polygon IDs.

build_feature_layer <- function(feature_type, PCT_set_to_use, current_ID_array, current_data_attributes, 
                                condition_class_vals, feature_params, condition_class_bounds, modify_means, means_modifier){
  
  if (length(PCT_set_to_use) == 0){
    current_feature = vector()
  } else {
    
    current_condition_class_set = current_data_attributes$condition[PCT_set_to_use] 
    current_object_ID_set = current_data_attributes$object_ID[PCT_set_to_use]
    current_feature = matrix(0, dim(current_ID_array)[1], dim(current_ID_array)[2])
    
    # running through all the polygon IDs
    for (ID_ind in seq_along(current_object_ID_set)){
      
      current_element_set = which(current_ID_array %in% current_object_ID_set[ID_ind])
      current_condition_class_mode = as.numeric(condition_class_vals[, 2][ match(current_condition_class_set[ID_ind], condition_class_vals[, 1]) ])
      
      if (feature_type == 'condition_Class'){
        #make the condition class layer (each polygon of veg is composed of elements with the same value)
        current_element_vals = rep(current_condition_class_mode, length(current_element_set))
        
      } else if (feature_type == 'Feature_Value'){
        
        # Call OSIM function to sample feature values given condition class and condition class bounds
        
        # Modify this condition_class_bounds is a nested list with info on the min, max and mean for each of the given conditions. 
        # Want to alter the mean values in condition_class_bounds to be able make 
        
        if (modify_means == TRUE){
          current_condition_class_bounds = lapply(seq_along(condition_class_bounds), 
                                                  function(i) modify_mean(condition_class_bounds[[i]],
                                                                          means_modifier[[ID_ind]]))
        } 
        
        current_element_vals = offsetsim::simulate_site_feature_elements(feature_params$site_sample_type,
                                                                         current_condition_class_mode,
                                                                         current_condition_class_bounds,
                                                                         element_num = length(current_element_set),
                                                                         feature_params$initial_site_sd, 
                                                                         feature_params$initial_site_mean_sd)
      }
      
      current_feature[current_element_set] = current_element_vals
      
    }
  } 
  return(current_feature)
}


modify_mean <- function(current_condition_class_bounds, current_mean_modifier){
  
  if (current_mean_modifier >= 0){
    modified_mean = current_condition_class_bounds[2] + current_mean_modifier*(current_condition_class_bounds[3] - current_condition_class_bounds[2])
  } else {
    modified_mean = current_condition_class_bounds[2] + current_mean_modifier*(current_condition_class_bounds[2] - current_condition_class_bounds[1])
  }
  
  current_condition_class_bounds = c(current_condition_class_bounds[1], modified_mean, current_condition_class_bounds[3])
  
}


calc_intervention_probability <- function(weight_layer, land_parcels, site_indexes_to_exclude){
  
  intervention_weights = rep(list(0), length(land_parcels))
  sites_to_use = setdiff(seq_along(land_parcels), site_indexes_to_exclude)
  intervention_weights[sites_to_use] = lapply(sites_to_use, function(i) mean(weight_layer[land_parcels[[i]]]))
  scale_factor = sum(unlist(intervention_weights))
  intervention_weights = lapply(seq_along(intervention_weights), function(i) intervention_weights[[i]]/scale_factor)
  
  return(intervention_weights)
}


project_data_to_zone_56 <- function(shp_to_project){
  # define projections
  GDA.D <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs ")
  GDA94.56 <- CRS("+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  shp_transform <- spTransform(shp_to_project, GDA94.56) # project to correct CRS
  return(shp_transform)
}


sim_time <- Sys.time()

source('cumberland_params.R')

# Function defined in cumberland_params.R
feature_params = initialise_user_feature_params()

build_params = list()
build_params$run_build_site_characteristics = FALSE
build_params$build_probability_list = FALSE
build_params$save_probability_list = FALSE

build_params$data_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/updated_rasters_jan_16/')
build_params$output_data_folder = '/Users/E24661/offset_data/Sydney_Cumberland_Data/new_prepared_data/'
build_params$data_attribute_folder = build_params$data_folder
build_params$simulation_inputs_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/simulation_inputs_new/')

# This is how the parcel size sampling is done Specify area size classes for
# sampling condition. If want to use different size classes, change this vector.
# This is based on the freq distribution of the shape_area values for each PGA
build_params$area_cuts_to_use = c(seq(0, 1e4, by = 1e3), Inf)

#set the minimum allow in each cut as set by build_params$area_cuts_to_use
build_params$min_cut_data_num = 10 


# For the current polygon of the given PCT of a given area, check if there are
# more than build_params$min_data_count entries to create the distriubtion to sample from
# for the size class. Otherwise sameple from all size calsses for that PCT, if
# still not enough add in all PCT of that particular cut.

build_params$min_data_count = 50

# only use PCT 849
build_params$PCT_to_use = 849 #unique(priority_condition_set$PCT)

build_params$condition_class_vals = matrix( ncol=2, byrow=TRUE, 
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


# Note that raster_to_array also removes all NAs and turns them into zeros.

build_params$data_filenames = setdiff(list.files(path = build_params$data_folder, pattern = ".tif", all.files = FALSE, include.dirs = FALSE, no.. = FALSE), 
                                      list.files(path = build_params$data_folder, pattern = ".tif.", all.files = FALSE, include.dirs = FALSE, no.. = FALSE))

# These are the raster tif files that match each of the files defined in
# priority_data_att_files. This is a rasterised version of the shape files and
# priority_data_att_files is a excel file of the attribute table.
build_params$priority_data_filenames = paste0(build_params$data_folder, "All_Vegetation_CumberlandPlain.tif")

# These are the names of the excel files which contain the detailed veg
# mapping by BIOSIS in the PGAs. These are the attributed tables of veg
# mapping done by BIOSIS provided as shape files.

build_params$priority_data_att_filenames = c('AllVegetation_CumberlandPlain')

cadastre_ind = which(build_params$data_filenames == "cadastre_withconstraints.tif")

GrowthAreas_ind = which(build_params$data_filenames == "GrowthAreas.tif")

data_rasters = setNames(lapply(seq_along(build_params$data_filenames), 
                               function(i) offsetsim::load_rasters(paste0(build_params$data_folder, build_params$data_filenames[i]), features_to_use = 'all')), build_params$data_filenames)

data_arrays = setNames(lapply(seq_along(build_params$data_filenames), function(i) offsetsim::raster_to_array(data_rasters[[i]])), build_params$data_filenames)

feature_ID_rasters = offsetsim::load_rasters(build_params$priority_data_filenames, features_to_use = 'all')

# Convert the feature_ID_rasters stack to a list of arrays, turning NAs to zeros.
# The values in the array represent the IDs for each polygon of Veg
feature_ID_layers = lapply(seq(dim(feature_ID_rasters)[3]), function(i) offsetsim::raster_to_array(subset(feature_ID_rasters, i)))

cumberland_layer = feature_ID_layers[[1]]*(1 - data_arrays$GrowthAreas_DevFootprint.tif)
cumberland_layer_IDs = unique(as.vector(cumberland_layer))

####### REMOVE IF NECESSARY - used to simulate two disjoint layers
feature_ID_layers = append(feature_ID_layers, list(cumberland_layer))

# Build the site characteristics object. Contains the info the simulation to
# assign pixel values to appropriate parcel and vegetation polygons. This
# object is a list of vectors and arrays etc...
# Site characteristics is a nested list that holds the ids of each parcel.
# It's the key that is used to break up the features in which bits overlap.
# You can save time by not generating this agin by setting this flag to FALSE.
# However if site_characteristics.rds isn't found, then it will be
# autmotically generated.
# This is a list of length n, where n is the total number of parcels as define
# in "cad_rst_exprt.tiff".  The IDs in this layer extrated via uique and sort,
# and then renumbered starting from zero. Thus the ID of parcels to remove
# need to mapped to correspond to this. 

if (build_params$run_build_site_characteristics == TRUE){
  cat('\nbuilding site_characteristics...')
  site_characteristics = offsetsim::build_site_characteristics(data_arrays[[cadastre_ind]])
  
  saveRDS(object = site_characteristics, file = paste0(build_params$simulation_inputs_folder, 'site_characteristics.rds'))
  
  paste0('site_characteristics object built at ',
         round(difftime(Sys.time(), sim_time), 1), 
         units(difftime(Sys.time(), sim_time)))
  
  cat('\nsaved site_characteristics object to ', paste0(build_params$simulation_inputs_folder, 'site_characteristics.rds'))
  
} else {
  if (file.exists(paste0(build_params$simulation_inputs_folder, 'site_characteristics.rds'))){
    paste0('loading site_characteristics object from file')
    site_characteristics = readRDS(paste0(build_params$simulation_inputs_folder, 'site_characteristics.rds'))
  }
}


# Note: all Site IDs are integers. The code above, finds all unique integes
# and gives then an ID starting from 1. However NAs have all gone to zero
# above and hence, all zero parcels will have a site index of "1". We want to
# remove them from the analysis as these aren't used. This is why
# site_indexes_to_exclude is set to "1" below.

if (build_params$build_probability_list == TRUE){
  
  layer_names_to_use = matrix( ncol=2, byrow=TRUE, 
                               c('GrowthAreas_DevFootprint.tif', 'dev_probability_list',
                                 'SelectedAReas_Draft_v1.tif', 'offset_probability_list',
                                 'Phase0Constraints.tif', 'offset_probability_list_phase0'))
  
  probability_list = setNames(lapply(seq(dim(layer_names_to_use)[1]), 
                                     function(i) calc_intervention_probability(data_arrays[[which(names(data_arrays) == layer_names_to_use[i, 1])]],
                                                                               site_characteristics$land_parcels, 
                                                                               site_indexes_to_exclude = 1)), 
                              layer_names_to_use[, 2])
  
  # Save the objects the output folder, save to file with the same name as the sublists (eg dev_probability_list)
  if (build_params$save_probability_list == TRUE){
    offsetsim::save_simulation_inputs(probability_list, build_params$simulation_inputs_folder)
  }
}



priority_data_attributes = vector('list', length(build_params$priority_data_att_filenames)) #vector of type list

# Read in data attributes associated with BIOSIS files from Veg mapping that were generated from ARCGIS
for (data_ind in seq_along(build_params$priority_data_att_filenames)){
  current_veg = build_params$priority_data_att_filenames[data_ind]
  priority_data_attributes[[data_ind]] = read.xls(paste0(build_params$data_attribute_folder, build_params$priority_data_att_filenames[data_ind], '.xls'))
}


# build artificial attribute table for region outside priority region - set all polygons outside priority region to cumberland_layer 
cumberland_ID_indexes = which(as.vector(priority_data_attributes[[1]]$object_ID) %in% cumberland_layer_IDs)
priority_IDs = setdiff(1:dim(priority_data_attributes[[1]])[1], cumberland_ID_indexes)

cumberland_att = priority_data_attributes[[1]][cumberland_ID_indexes, ]
priority_data_attributes[[1]] = priority_data_attributes[[1]][priority_IDs, ]

#------------------------------
# Initialize the veg condition 
#------------------------------

# This selects a subset priority_data_attributes that only contain the 'PCT', 'condition', 'shape_area'.

priority_condition_att = lapply(seq_along(priority_data_attributes), 
                                function(i) priority_data_attributes[[i]][match(c('PCT', 'condition', 'shape_area'), names(priority_data_attributes[[i]]))])

# makes one matrix of the data attributes. All priority areas are combined into one single matrix.
priority_condition_set = do.call("rbind", priority_condition_att)

labels_to_use = seq(length(build_params$area_cuts_to_use) - 1)
priority_cuts_by_PCT = vector('list', length(build_params$PCT_to_use))

# Setting up wher the cuts are. For each PCT polygon, check which cut category the polygon should go into.
for (PCT_ind in seq_along(build_params$PCT_to_use)){
  current_PCT_set = (priority_condition_set$PCT == build_params$PCT_to_use[PCT_ind])
  
  current_cuts = cut(priority_condition_set$shape_area[current_PCT_set], build_params$area_cuts_to_use, labels = labels_to_use)
  
  current_condition_set = priority_condition_set$condition[current_PCT_set]
  current_cut_set = vector('list', length(labels_to_use))
  
  for (cut_ind in seq_along(labels_to_use)){
    current_cut_set[[cut_ind]] = as.character(current_condition_set[current_cuts == cut_ind])
  }
  
  priority_cuts_by_PCT[[PCT_ind]] = current_cut_set
  
}


# Splits the data up via the specified cuts
full_cuts = cut(priority_condition_set$shape_area, build_params$area_cuts_to_use, labels = labels_to_use)

full_priority_cut_stats = vector('list', length(labels_to_use))

# bulding the distribution to sample from. Note the 'table' command give the fequency of each element 
# note - check the full_priority_cut_stats to see what the data is like - if frequency is too low, enlarge cuts 
# by removing some of the cut points

cuts_to_remove = vector()

for (cut_ind in seq_along(labels_to_use)){
  full_priority_cut_stats[[cut_ind]] = as.data.frame(table(priority_condition_set$condition[full_cuts == cut_ind]))
  if (sum(full_priority_cut_stats[[cut_ind]]$Freq) < build_params$min_cut_data_num){
    cuts_to_remove <- append(cuts_to_remove, cut_ind)
  }
}

# flag error where
if (length(cuts_to_remove) > 0){
  print(paste0('area_cuts_to_use is poorly parametrised, remove', paste(cuts_to_remove)))
  stop()
}

cumberland_cuts = as.numeric(cut(cumberland_att$shape_area, build_params$area_cuts_to_use, labels = labels_to_use))
cumberland_conditions = vector(mode ="character", length(cumberland_att$shape_area))


# This loop here now does the sampling for each polygon of each PCT. For now
# we are just using PCT 849. this builds a data attribute table for the polygons
# that don't have attributes for condition, based on the Biosis condition
# classes in the PGAs


for (site_ind in seq_along(cumberland_att$shape_area)){
  # see if this is a polygon of veg is one the relevant PCTs
  if (cumberland_att$PCT[site_ind] %in% build_params$PCT_to_use){
    
    # data frame with with polygons from the PGAs, of the current cut category within the PCT
    current_priority_stats = as.data.frame(table(priority_cuts_by_PCT[[match(cumberland_att$PCT[site_ind], build_params$PCT_to_use)]][[cumberland_cuts[site_ind]]]))
    # check if there are enough polygons to sample from
    if (sum(current_priority_stats$Freq) < build_params$min_data_count){
      # An this case there is not enough, so expland out to use all cut categoires for that PCT
      current_priority_stats = as.data.frame(table(unlist(priority_cuts_by_PCT[[match(cumberland_att$PCT[site_ind], build_params$PCT_to_use)]])))
    }
    # if still not enough data to sampel from 
    if (sum(current_priority_stats$Freq) < build_params$min_data_count){
      # sample from all PCTs within the cut category
      current_priority_stats = full_priority_cut_stats[[cumberland_cuts[site_ind]]]
    }
  } else {
    # Here we are one of the PCTs not in PCT_to_use, in this case sample from all the data.89
    current_priority_stats = full_priority_cut_stats[[cumberland_cuts[site_ind]]]
    # This is the sampling to determine the condition class
    
  }
  
  if (sum(current_priority_stats$Freq) > 0){
    current_sampled_condition_index = sample(x = seq_along(current_priority_stats$Freq), size = 1, 
                                             prob = current_priority_stats$Freq/sum(current_priority_stats$Freq), replace = TRUE)
  } else {
    print('poor cut parametrisation flag')
  }
  # This builds a data attributes list that matches the condition_class_vlas
  # Storing the condition class based on what is in condition_class_vals above (eg in tact, scattered trees et)
  
  # building a vector of names, based on the sampled conditon value of each polygon
  # Note "Var1" is from R's table() function call above.
  cumberland_conditions[site_ind] = as.character(current_priority_stats$Var1[current_sampled_condition_index])
}

# assign all the new sampled condition classes to attribute table of the cumberland plain veg shape file (that did not originally have veg data)
cumberland_att$condition = cumberland_conditions

# Binds the cumberland_att with new condition info to the PGA Biosis data
data_attributes = append(priority_data_attributes, list(cumberland_att))

# sets the list names to match the veg layers
names(data_attributes) = c(build_params$priority_data_att_filenames, 'cumberland_att')


# ----------------------
# Building the condition class array which gives the condition class of each pixel to then write them to raster layers 
# ----------------------


# Builds a nested list defined by the 4 data layers and of length PCT_to_use  
# to save info for each PCT separately.

print('building condition classes')
condition_class_set = lapply(seq_along(data_attributes), function(i) vector('list', length(build_params$PCT_to_use)))

for (data_ind in seq_along(data_attributes)){
  
  for (PCT_ind in seq_along(build_params$PCT_to_use)){
    
    PCT_set_to_use = which(data_attributes[[data_ind]]$PCT == build_params$PCT_to_use[PCT_ind])
    
    # Takes the data attributes table together with the polygon IDs and turns them into a matrix (to be written as a raster file later)
    # Note, the feature_type = 'condition_Class', specified that this function will be sampling from condition classes
    
    current_feature = build_feature_layer(feature_type = 'condition_Class', 
                                          PCT_set_to_use, 
                                          current_ID_array = feature_ID_layers[[data_ind]], 
                                          current_data_attributes = data_attributes[[data_ind]], 
                                          build_params$condition_class_vals, 
                                          feature_params, 
                                          condition_class_bounds = vector(), 
                                          modify_means = FALSE, 
                                          means_modifier = vector())
    
    # This dealing with the overlaps between the Biosis PGA veg mapping and the Cumberland west veg mapping
    # for the overlap area use the biosis values
    
    if (names(data_attributes)[data_ind] ==  'cumberland_att'){
      # outside PGAs so use sampled data
      current_feature = current_feature*(1 - data_arrays[[GrowthAreas_ind]])
    } else {
      # Use Biosis data
      current_feature = current_feature*data_arrays[[GrowthAreas_ind]]
    }
    condition_class_set[[data_ind]][[PCT_ind]] = current_feature
    
  }
  
  paste0('data attribute ', data_ind, ' completed')
  
}


  
paste0('condition classes built at ',
       round(difftime(Sys.time(), sim_time), 1), 
       units(difftime(Sys.time(), sim_time)))


# for each PCT, add the PGA condition array to the sampled condition array.

merged_condition_classes = vector('list', length(build_params$PCT_to_use))

for (PCT_ind in seq_along(build_params$PCT_to_use)){
  inds_to_use = which(unlist(lapply(seq_along(condition_class_set), function(i) length(condition_class_set[[i]][[PCT_ind]]) > 0)))
  merged_condition_classes[[PCT_ind]] = Reduce('+', lapply(inds_to_use, function(i) condition_class_set[[i]][[PCT_ind]]))
}


# write out the condition class rasters that are used by the simulation to split features per condition class per site
# if these are not supplied the simulation will assign the condition classes internally - which is a problem for this project

for (PCT_ind in seq_along(build_params$PCT_to_use)){
  current_feature_raster = raster(merged_condition_classes[[PCT_ind]])
  for (feature_ind in seq(feature_params$simulated_feature_num)){
    current_file_name = paste0(build_params$output_data_folder, 'PCT_', build_params$PCT_to_use[PCT_ind], '_condition_class_', 
                               formatC(feature_ind, width = 3, format = "d", flag = "0"), '.tif')
    writeRaster(current_feature_raster, current_file_name, overwrite = TRUE)
  }
}


print('building condition values')
# ----------------------
# Building the condition value array which gives the condition value of each pixel to then write them to raster layers for each feature 
# ----------------------



# Same issue as above about combining the PGA data with the data fro the rest of the Cumberland mapping.

# This is the vector you need to supply to yield higher feature values, and subsequently their matching feature dynamics. It needs to be of equal length
# to the number of feature polygons in the current PCT, in this case defined by length(which(data_attributes[[data_ind]]$PCT == build_params$PCT_to_use)). All
# values within vector range between [-1, 1] and are used to modify the distribution used to sample the feature values from.
# Values in range [0, 1] increase the mean, values in range [-1, 0] decrease the mean. Therefore if you identify large (conservation) regions with values 
# between [0, 1], and regions with poor connectivity between [-1, 0] you will get the effect that grouping offsets together will yield betetr results that 
#splitting them apart. Note that this would ideally be calculated dynamically in situ as the simulation is running to account for the changes in the distribution
# of sites as the simulation proceeds

means_modifier = runif(min = -1, max = 1, length(which(data_attributes[[data_ind]]$PCT == build_params$PCT_to_use)))

condition_vals_set = lapply(seq_along(data_attributes), 
                            function(i) lapply(seq_along(build_params$PCT_to_use), 
                                               function(j) vector('list', feature_params$simulated_feature_num)))

for (data_ind in seq_along(data_attributes)){
  
  for (PCT_ind in seq_along(build_params$PCT_to_use)){
    PCT_set_to_use = which(data_attributes[[data_ind]]$PCT == build_params$PCT_to_use[PCT_ind])
    
    for (feature_ind in seq(feature_params$simulated_feature_num)){
      
      # Note, the feature_type = 'Feature_Value', means sampling actual condtion values for each pixel.
      current_feature = build_feature_layer(feature_type = 'Feature_Value', 
                                            PCT_set_to_use, 
                                            current_ID_array = feature_ID_layers[[data_ind]], 
                                            current_data_attributes = data_attributes[[data_ind]], 
                                            build_params$condition_class_vals, 
                                            feature_params, 
                                            # Note these are where the condition class bounds come from 
                                            condition_class_bounds = feature_params$initial_condition_class_bounds[[feature_ind]], 
                                            modify_means = TRUE, 
                                            means_modifier)
      
      if (names(data_attributes)[data_ind] ==  'cumberland_att'){
        current_feature = current_feature*(1 - data_arrays[[GrowthAreas_ind]])
      } else {
        current_feature = current_feature*data_arrays[[GrowthAreas_ind]]
      }
      
      condition_vals_set[[data_ind]][[PCT_ind]][[feature_ind]] = current_feature
    }
    paste0('feature ', feature_ind, 'values built at ',
           round(difftime(Sys.time(), sim_time), 1), 
           units(difftime(Sys.time(), sim_time)))
  }
}


  


paste0('condition values built at ',
       round(difftime(Sys.time(), sim_time), 1), 
       units(difftime(Sys.time(), sim_time)))

# combining the values

merged_condition_vals = lapply(seq_along(build_params$PCT_to_use), function(i) vector('list', feature_params$simulated_feature_num ))

for (PCT_ind in seq_along(build_params$PCT_to_use)){
  for (feature_ind in seq(feature_params$simulated_feature_num)){
    inds_to_use = which(unlist(lapply(seq_along(condition_class_set), function(i) length(condition_vals_set[[i]][[PCT_ind]][[feature_ind]]) > 0)))
    merged_condition_vals[[PCT_ind]][[feature_ind]] = Reduce('+', lapply(inds_to_use, function(i) condition_vals_set[[i]][[PCT_ind]][[feature_ind]]))
  }
}


# writing them as rasters.

for (PCT_ind in seq_along(build_params$PCT_to_use)){
  for (feature_ind in seq(feature_params$simulated_feature_num)){
    current_feature_raster = raster(merged_condition_vals[[PCT_ind]][[feature_ind]])
    current_file_name = paste0(build_params$output_data_folder, 'PCT_', build_params$PCT_to_use[PCT_ind], '_feature_', 
                               formatC(feature_ind, width = 3, format = "d", flag = "0"), '.tif')
    writeRaster(current_feature_raster, current_file_name, overwrite = TRUE)
    print(current_file_name) 
  }
}


paste0('all routines done at ',
       round(difftime(Sys.time(), sim_time), 1), 
       units(difftime(Sys.time(), sim_time)))

