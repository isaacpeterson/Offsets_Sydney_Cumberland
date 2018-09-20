rm(list = ls())
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(abind)
library(pixmap)
library(offsetsim)
library(gdata)


# This function says: I have a data attributes table, take the data attributes table and info on
# current_ID_array array of polygon IDs.
build_feature_layer <- function(feature_type, PCT_set_to_use, current_ID_array, current_data_attributes, condition_class_vals, feature_params, condition_class_bounds){
  
  if (length(PCT_set_to_use) == 0){
    current_feature = vector()
  } else {
    current_condition_class_set = data_attributes[[data_ind]]$Condition[PCT_set_to_use] 
    current_object_ID_set = data_attributes[[data_ind]]$OBJECTID[PCT_set_to_use]
    
    current_feature = matrix(0, dim(current_ID_array)[1], dim(current_ID_array)[2])
    
    # running through all the polygon IDs
    for (ID_ind in seq_along(current_object_ID_set)){
      
      current_element_set = which(current_ID_array %in% current_object_ID_set[ID_ind])
      current_condition_class_modes = as.numeric(condition_class_vals[, 2][ match(current_condition_class_set[ID_ind], condition_class_vals[, 1]) ])
      
      if (feature_type == 'Condition_Class'){
        #make the condition class layer (each polygon of veg is composed of elements with the same value)
        current_element_vals = rep(current_condition_class_modes, length(current_element_set))
      } else if (feature_type == 'Feature_Value'){
        
        # Call OSIM function to sample feature values given condition class and condition class bounds

        # Modify this condition_class_bounds is a nested list with info on the min, max and mean for each of the given conditions. 
        # Want to alter the mean values in condition_class_bounds to be able make 

        current_element_vals = offsetsim::simulate_site_feature_elements(feature_params$site_sample_type,
                                                                         current_condition_class_modes,
                                                                         condition_class_bounds,
                                                                         element_num = length(current_element_set),
                                                                         feature_params$initial_site_sd, 
                                                                         feature_params$initial_site_mean_sd)
      }
      
      current_feature[current_element_set] = current_element_vals
      
    }
  } 
  return(current_feature)
}







build_probability_list <- function(weight_layer, land_parcels, site_indexes_to_exclude){

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
  

# source('cumberland_params.R')
source('/Users/ascelin/analysis/GitHub/Offsets_Sydney_Cumberland/cumberland_params.R')


# Function defined in cumberland_params.R
feature_params = initialise_user_feature_params()

# Site characteristics is a nested list that holds the ids of each parcel.
# It's the key that is used to break up the features in which bits overlap.
# You can save time by not generating this agin by setting this flag to FALSE.
# However if site_characteristics.rds isn't found, then it will be
# autmotically generated.
overwrite_site_characteristics = FALSE


# Isaac's locations 
# data_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/cumberland_aligned_tif/')
# output_data_folder = '/Users/E24661/offset_data/Sydney_Cumberland_Data/prepared_data/'
# data_attribute_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/veg_attributes/')
# simulation_inputs_folder = paste0(path.expand('~'), '/offset_data/Sydney_Cumberland_Data/simulation_inputs/')

# Ascelin's locations 
data_folder = paste0(path.expand('~'), '/analysis/offset_simulator/Sydney_Cumberland_Data/cumberland_aligned_tif/')
output_data_folder = '/Users/ascelin/analysis/offset_simulator/Sydney_Cumberland_Data/testing3/output_data_folder/'
data_attribute_folder = paste0(path.expand('~'), '/analysis/offset_simulator/Sydney_Cumberland_Data/veg_attributes/')
simulation_inputs_folder = paste0(path.expand('~'), '/analysis/offset_simulator/Sydney_Cumberland_Data/testing3/simulation_inputs_folder/')

# These are the names of the excel files which contain the detailed veg
# mapping by BIOSIS in the PGAs. These are the attributed tables of veg
# mapping done by BIOSIS provided as shape files.
# I think these are the shapefiles in the following: <>Sydney_Cumberland_Data/Projected_Cumberland_data/projected_macarthur.shp etc
priority_data_att_files = c('mac_veg_att', 'west_veg_att', 'wilton_veg_att')

# These are the raster tif files that match each of the files defined in
# priority_data_att_files. This is a rasterised version of the shape files and
# priority_data_att_files is a excel file of the attribute table.
priority_data_filenames = c("mac_veg_rst_exprt.tif", "west_veg_rst_exprt.tif", "wilt_veg_rst_exprt.tif")


# Makes a raster stack of 4 layers: the Biosis veg mapping from the 3 PGAs
# (mac_veg_rst_exprt.tif, west_veg_rst_exprt.tif, wilt_veg_rst_exprt.tif), as
# well as cum_ID_rast1.tif, which is currently the cumberland West veg data,
# but needs to have the Sydney Metro veg data added to it.
feature_ID_layers = offsetsim::load_rasters(paste0(data_folder, c(priority_data_filenames, "cum_ID_rast1.tif")), features_to_use = 'all')

# Converss the stack of 4 layers to a list of arrarys, turning NAs to zeros.
# The values in the array represent the IDs for each polygon of Veg
feature_ID_layers = lapply(seq(dim(feature_ID_layers)[3]), function(i) offsetsim::raster_to_array(subset(feature_ID_layers, i)))

# Raster layer with the parcel ID in each pixel. 
# Note that raster_to_array also removes all NAs and turns them into zeros.
cadastre = offsetsim::raster_to_array(offsetsim::load_rasters(paste0(data_folder, "cad_rst_exprt.tif"), features_to_use = 'all'))

# Defines the study area and excludes parcels outside the study region. This
# could be used to exclude additional areas from the analysis. Note for now all parcels are includeds.
cadastre_msk = offsetsim::raster_to_array(offsetsim::load_rasters(paste0(data_folder, "cum_sub_rst_exprt.tif"), features_to_use = 'all'))

# This should be a 0/1 raster that define the PGAs.
PGA_msk = offsetsim::raster_to_array(offsetsim::load_rasters(paste0(data_folder,"priority_rast_exprt.tif"), features_to_use = 'all')) > 0


# Build the site characteristics object. Contains the info the simulation to
# assign pixel values to appropriate parcel and vegetation polygons. This
# object is a list of vectors and arrarys etc...
if (file.exists(paste0(simulation_inputs_folder, 'site_characteristics.rds')) & (overwrite_site_characteristics == FALSE)){
  site_characteristics = readRDS(paste0(simulation_inputs_folder, 'site_characteristics.rds'))
} else {
  cat('\nBuilding site_characteristics...')
  cadastre_array = cadastre*cadastre_msk 
  site_characteristics = offsetsim::build_site_characteristics(cadastre_array)
  saveRDS(object = site_characteristics, file = paste0(simulation_inputs_folder, 'site_characteristics.rds'))
  cat('\nWrote site_characteristics.rds')
}



# Note - the two masks dont perfectly overlap - hence enforce cadastre msk boundaries - otherwise redefine cadastre_msk in ARCGIS
# Set to zero any areas that zero in either layer.

# note there were some bits of the PGAs that went slightly outside the study
# area so might need to deal with this if don't want to exclude some small
# areas of the PGA.

PGA_msk = PGA_msk * cadastre_msk

dev_msk = PGA_msk * cadastre_msk


# Note - the two masks dont perfectly overlap - hence enforce cadastre msk boundaries - otherwise redefine cadastre_msk in ARCGIS
# Set to zero any areas that zero in either layer.
# Note: if I want exclude exisiting conservation areas for offsets, I would add them in here. 
offset_region = (!dev_msk*cadastre_msk)

# Currently unregulated loss can occur anywhere in the study area
unregulated_loss_region = cadastre_msk


objects_to_save = list()

# Note: all Site IDs are integers. The code above, finds all unique integes
# and gives then an ID starting from 1. However NAs have all gone to zero
# above and hence, all zero parcels will have a site index of "1". We want to
# remove them from the analysis as these aren't used. This is why
# site_indexes_to_exclude is set to "1" below.

# This line of code excludes site index "1" as the current setup rewrites zeros for NA vals in
# cadastre array, resulting in the NA's being listed as "0", and
# correspondingly  the first site is given all of these elements.

# If there are any other parcels that are to be excluded from development or offset regions,
# just pass in a vector of parcel IDs as the site_indexes_to_exclude parameter
# of this function call

# This is a list of length n, where n is the total number of parcels as define
# in "cad_rst_exprt.tiff".  The IDs in this layer extrated via uique and sort,
# and then renumbered starting from zero. Thus the ID of parcels to remove
# need to mapped to correspond to this. 
# Each element of the list is a single number.
objects_to_save$dev_probability_list = build_probability_list(dev_msk, site_characteristics$land_parcels, site_indexes_to_exclude = 1)
objects_to_save$offset_probability_list = build_probability_list(offset_region, site_characteristics$land_parcels, site_indexes_to_exclude = 1)
objects_to_save$unregulated_probability_list = build_probability_list(unregulated_loss_region, site_characteristics$land_parcels, site_indexes_to_exclude = 1)

# Save the objects the output folder, save to file with the same name as the sublists (eg dev_probability_list)
offsetsim::save_simulation_inputs(objects_to_save, output_data_folder)


   #------------------------------
   # Initialize the veg condition 
   #------------------------------


data_num = length(priority_data_att_files)
priority_data_attributes = vector('list', data_num) #vector of type list

# Read in data attributes associated with BIOSIS files from Veg mapping that were generated from ARCGIS
for (data_ind in seq(data_num)){
  current_veg = priority_data_att_files[data_ind]
  priority_data_attributes[[data_ind]] = read.xls(paste0(data_attribute_folder, priority_data_att_files[data_ind], '.xls'))
}


# This selects a subset priority_data_attributes that only contain the 'PCT', 'Condition', 'Shape_Area'.
priority_condition_att = lapply(seq_along(priority_data_attributes), 
        function(i) priority_data_attributes[[i]][match(c('PCT', 'Condition', 'Shape_Area'), names(priority_data_attributes[[i]]))])

# Binds the corresponding list elements into arrys with the info.

# makes one 3xn matrix of the data attributes. All priority areas are combined into one single matrix.
priority_condition_set = do.call("rbind", priority_condition_att)

# Attribute file that contains polygon information on cumberland region 
cumberland_att = read.xls(paste0(data_attribute_folder, 'cum_veg_att.xls'))

# This is how the parcel size sampling is done Specify area size classes for
# sampling condition. If want to use different size classes, change this
# vector.
# This is based on the freq distribution of the Shape_Area values for each PGA
area_cuts_to_use = c(seq(0, 1e5, by = 1e4), Inf)

labels_to_use = seq(length(area_cuts_to_use) - 1)


# only use PCT 849
PCT_to_use = 849 #unique(priority_condition_set$PCT)
#feature_num = 5 # number of features to use in condtion modelling (TG, GG) # to delete


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


# build list
priority_cuts_by_PCT = vector('list', length(PCT_to_use))


# Setting up wher the cuts are. For each PCT polygon, check which cut category the polygon should go into.
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


# Splits the data up via the specified cuts
full_cuts = cut(priority_condition_set$Shape_Area, area_cuts_to_use, labels = labels_to_use)

full_priority_cut_stats = vector('list', length(labels_to_use))

# bulding the distribution to sample from. Note the 'table' command give the fequency of each element 
for (cut_ind in seq(length(labels_to_use))){
  full_priority_cut_stats[[cut_ind]] = as.data.frame(table(priority_condition_set$Condition[full_cuts == cut_ind]))
}

cumberland_cuts = cut(cumberland_att$Shape_Area, area_cuts_to_use, labels = labels_to_use)
cumberland_conditions = vector(mode="character", length(cumberland_att$Shape_Area))



# For the current polygon of the given PCT of a given area, check if there are
# more than min_data_count entries to create the distriubtion to sample from
# for the size class. Otherwise sameple from all size calsses for that PCT, if
# still not enough add in all PCT of that particular cut.
min_data_count = 50


# This loop here now does the sampling for each polygon of each PCT. For now
# we just using PCT 849. this builds a data attribute table for the polygons
# that don't have attributes for condition, based on the Biosis condition
# classes in the PGAs

for (site_ind in seq(length(cumberland_att$Shape_Area))){
  
  # see if this is a polygon of veg is one the relevant PCTs
  if (cumberland_att$PCT[site_ind] %in% PCT_to_use){

    # data frame with with polygons from the PGAs, of the current cut category within the PCT
    current_priority_stats = as.data.frame(table(priority_cuts_by_PCT[[match(cumberland_att$PCT[site_ind], PCT_to_use)]][[cumberland_cuts[site_ind]]]))

    # check if there are enough polygons to sample from
    if (sum(current_priority_stats$Freq) < min_data_count){

      # An this case there is not enough, so expland out to use all cut categoires for that PCT
      current_priority_stats = as.data.frame(table(unlist(priority_cuts_by_PCT[[match(cumberland_att$PCT[site_ind], PCT_to_use)]])))
    }

    # if still not enough data to sampel from 
    if (sum(current_priority_stats$Freq) < min_data_count){

      # sample from all PCTs within the cut category
      current_priority_stats = full_priority_cut_stats[[cumberland_cuts[site_ind]]]

    }

  } else {

    # Here we are one of the PCTs not in PCT_to_use, in this case sample from all the data.
    current_priority_stats = full_priority_cut_stats[[cumberland_cuts[site_ind]]]
  }
  

  # This is the sampling to determine the condition class
  current_cond_inds = sample(x = seq_along(current_priority_stats$Freq), size = 1, prob = current_priority_stats$Freq/sum(current_priority_stats$Freq), replace = TRUE)

  # This builds a data attributes list that matches the condition_class_vlas
  # Storing the condition class based on what is in condition_class_vals above (eg in tact, scattered trees et)

  # building a vector of names, based on the sampled conditon value of each polygon
  # Note "Var1" is from R's table() function call above.
  cumberland_conditions[site_ind] = as.character(current_priority_stats$Var1[as.numeric(current_cond_inds)])
}


# assign all the new sampled condition classes to attribute table of the cumberland plain veg shape file (that did not originally have veg data)
cumberland_att$Condition = cumberland_conditions

# Binds the cumberland_att with new condition info to the PGA Biosis data
data_attributes = append(priority_data_attributes, list(cumberland_att))


# sets the list names to match the veg layers
names(data_attributes) = c(priority_data_att_files, 'cum_veg_att')


# Builds a nested list defined by the 4 data layers and of length PCT_to_use  
# to save info for each PCT separately.
condition_class_set = lapply(seq_along(data_attributes), function(i) vector('list', length(PCT_to_use)))


# ----------------------
# Building the condition class arrary which gives the condition class of each pixel to then write them to raster layers 
# ----------------------

for (data_ind in seq_along(data_attributes)){
  
  for (PCT_ind in seq_along(PCT_to_use)){
    
    PCT_set_to_use = which(data_attributes[[data_ind]]$PCT == PCT_to_use[PCT_ind])

    # Takes the data attributes table together with the polygon IDs and turns them into a matrix (to be written as a raster file later)
    # Note, the feature_type = 'Condition_Class', specified that this function will be sampling from condition classes

    current_feature = build_feature_layer(feature_type = 'Condition_Class', 
                                          PCT_set_to_use, 
                                          current_ID_array = feature_ID_layers[[data_ind]], 
                                          current_data_attributes = data_attributes[[data_ind]], 
                                          condition_class_vals, 
                                          feature_params, 
                                          condition_class_bounds = vector())
    
    # This dealing with the overlaps between the Biosis PGA veg mapping and the Cumberland west veg mapping
    # for the overlap area use the biosis values

    if (names(data_attributes)[data_ind] ==  'cum_veg_att'){
      # outside PGAs so use sampled data
      current_feature = current_feature*(!PGA_msk)
    } else {
      # Use Biosis data
      current_feature = current_feature*PGA_msk
    }
    condition_class_set[[data_ind]][[PCT_ind]] = current_feature

  }
}


# for each PCT, add the PGA condition arrary to the sampled condition array.

merged_condition_classes = vector('list', length(PCT_to_use))
for (PCT_ind in seq_along(PCT_to_use)){

  inds_to_use = which(unlist(lapply(seq_along(condition_class_set), function(i) length(condition_class_set[[i]][[PCT_ind]]) > 0)))
  merged_condition_classes[[PCT_ind]] = Reduce('+', lapply(inds_to_use, function(i) condition_class_set[[i]][[PCT_ind]]))
}


# write out the condition class rasters that are used by the simulation to split features per condition class per site
# if these are not supplied the simulation will assign the condition classes internally - which is a problem for this project

for (PCT_ind in seq_along(PCT_to_use)){
  current_feature_raster = raster(merged_condition_classes[[PCT_ind]])
  for (feature_ind in seq(feature_params$simulated_feature_num)){
    current_file_name = paste0(output_data_folder, 'PCT_', PCT_to_use[PCT_ind], '_condition_class_', 
                               formatC(feature_ind, width = 3, format = "d", flag = "0"), '.tif')
    writeRaster(current_feature_raster, current_file_name, overwrite = TRUE)
  }
}



# ----------------------
# Building the condition value arrary which gives the condition value of each pixel to then write them to raster layers for each feature 
# ----------------------



# Same issue as above about combining the PGA data with the data fro the rest of the Cumberland mapping.


condition_vals_set = lapply(seq_along(data_attributes), 
                            function(i) lapply(seq_along(PCT_to_use), 
                                               function(j) vector('list', feature_params$simulated_feature_num)))

for (data_ind in seq_along(data_attributes)){
  
  for (PCT_ind in seq_along(PCT_to_use)){
    PCT_set_to_use = which(data_attributes[[data_ind]]$PCT == PCT_to_use[PCT_ind])
    
    for (feature_ind in seq(feature_params$simulated_feature_num)){
      
      # Note, the feature_type = 'Feature_Value', means sampling actual condtion values for each pixel.
      current_feature = build_feature_layer(feature_type = 'Feature_Value', 
                                            PCT_set_to_use, 
                                            current_ID_array = feature_ID_layers[[data_ind]], 
                                            current_data_attributes = data_attributes[[data_ind]], 
                                            condition_class_vals, 
                                            feature_params, 

                                            # Note these are where the condition class bounds come from 
                                            condition_class_bounds = feature_params$initial_condition_class_bounds[[feature_ind]])
      
      if (names(data_attributes)[data_ind] ==  'cum_veg_att'){
        current_feature = current_feature*(PGA_msk == 0)
      } else {
        current_feature = current_feature*PGA_msk
      }
      
      condition_vals_set[[data_ind]][[PCT_ind]][[feature_ind]] = current_feature
    }
    
  }
}



# combining the values

merged_condition_vals = lapply(seq_along(PCT_to_use), function(i) vector('list', feature_params$simulated_feature_num ))
                               
for (PCT_ind in seq_along(PCT_to_use)){
  for (feature_ind in seq(feature_params$simulated_feature_num)){
    inds_to_use = which(unlist(lapply(seq_along(condition_class_set), function(i) length(condition_vals_set[[i]][[PCT_ind]][[feature_ind]]) > 0)))
    merged_condition_vals[[PCT_ind]][[feature_ind]] = Reduce('+', lapply(inds_to_use, function(i) condition_vals_set[[i]][[PCT_ind]][[feature_ind]]))
  }
}


# writing them as rasters.

for (PCT_ind in seq_along(PCT_to_use)){
  for (feature_ind in seq(feature_params$simulated_feature_num)){
    current_feature_raster = raster(merged_condition_vals[[PCT_ind]][[feature_ind]])
    current_file_name = paste0(output_data_folder, 'PCT_', PCT_to_use[PCT_ind], '_feature_', 
                               formatC(feature_ind, width = 3, format = "d", flag = "0"), '.tif')
    writeRaster(current_feature_raster, current_file_name, overwrite = TRUE)
    print(current_file_name) 
  }
}




