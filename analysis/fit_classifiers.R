#----------------------------------------------
# This script sets out to produce
# classifiers for each problem and
# feature set
#
# NOTE: setup.R and calculate_feats_all_probs.R 
# must be run first
#----------------------------------------------

#------------------------------------
# Author: Trent Henderson, 7 May 2021
#------------------------------------

# Read in feature calculations

load("data/featureCalcs.Rda")

# Fix Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

#---------------- Fit classifiers ----------------

#' Function to automatically run all classifiers by feature set and problem
#' 
#' @param data the dataframe containing feature matrices and groups to use
#' @return a dataframe with class-balanced classification accuracy by problem and feature set
#' @author Trent Henderson
#' 

run_all_classifiers <- function(data){
  
  x
  
}
