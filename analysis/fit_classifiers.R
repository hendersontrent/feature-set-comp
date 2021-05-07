#------------------------------------
# This script sets out to produce
# classifiers for each problem and
# feature set
#
# NOTE: setup.R must be run first
#------------------------------------

#------------------------------------
# Author: Trent Henderson, 7 May 2021
#------------------------------------

# Read in feature calculations

if(!exists("data/featureCalcs.Rda")){
  source("analysis/calculate_feats_all_probs.R")
}

if(exists("data/featureCalcs.Rda")){
  load("data/featureCalcs.Rda")
}

#---------------- Fit classifiers ----------------


