#------------------------------------
# This script sets out to calculate
# time-series features for each
# Empirical 1000 dataset and retain
# them for analytical use
#
# NOTE: setup.R must be run first
#------------------------------------

#---------------------------------------
# Author: Trent Henderson, 9 August 2021
#---------------------------------------

# Read in data

load("data/empirical1000.Rda")

# Fix Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

#----------------- Calculate features -----------------

#' Function to automatically calculate all features for all datasets and feature sets
#' 
#' @param data the data containing raw time series for each dataset
#' @return a dataframe object containing the computed feature matrices
#' @author Trent Henderson
#' 

calculate_feats_Emp1000 <- function(data){
  
  message("Running catch22")
  try(outs_22 <- calculate_features(data = data, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "catch22"))
  message("Running feasts")
  try(outs_fe <- calculate_features(data = data, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "feasts"))
  message("Running tsfeatures")
  try(outs_ts <- calculate_features(data = data, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "tsfeatures"))
  message("Running tsfresh")
  try(outs_tsfresh <- calculate_features(data = data, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "tsfresh", tsfresh_cleanup = FALSE))
  message("Running TSFEL")
  try(outs_tsfel <- calculate_features(data = data, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "tsfel"))
  message("Running Kats")
  try(outs_kats <- calculate_features(data = data, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "kats"))
      
  # Bind all together
      
  outputData <- data.frame()
      
  if(exists("outs_22")){
    outputData <- dplyr::bind_rows(outputData, outs_22)
  }
  
  if(exists("outs_fe")){
    outputData <- dplyr::bind_rows(outputData, outs_fe)
  }
  
  if(exists("outs_ts")){
    outputData <- dplyr::bind_rows(outputData, outs_ts)
  }
  
  if(exists("outs_tsfresh")){
    outputData <- dplyr::bind_rows(outputData, outs_tsfresh)
  }
  
  if(exists("outs_tsfel")){
    outputData <- dplyr::bind_rows(outputData, outs_tsfel)
  }
  
  if(exists("outs_kats")){
    outputData <- dplyr::bind_rows(outputData, outs_kats)
  }
  
  return(outputData)
}

Emp1000FeatMat <- calculate_feats_Emp1000(data = empirical1000)

# Save as .Rda

save(Emp1000FeatMat, file = "data/Emp1000FeatMat.Rda")
