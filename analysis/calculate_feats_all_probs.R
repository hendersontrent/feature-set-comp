#------------------------------------
# This script sets out to calculate
# time-series features for each
# classification problem and retain
# them for classification use
#
# NOTE: setup.R must be run first
#------------------------------------

#------------------------------------
# Author: Trent Henderson, 7 May 2021
#------------------------------------

# Fix Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

#----------------- Calculate features -----------------

#' Function to automatically calculate all features for all datasets and feature sets
#' 
#' @param data the data containing raw time series for each problem
#' @return a dataframe object containing the computed feature matrices
#' @author Trent Henderson
#' 

calculate_feats_all_probs <- function(data){
  
  x
  
}

#----------------- Save features ----------------------

# Merge together



# Save as .Rda

save(featureCalcs, file = "data/featureCalcs.Rda")
