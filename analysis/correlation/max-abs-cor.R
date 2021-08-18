#--------------------------------------
# This script sets out to produce
# a matrix visualisation of the max abs
# correlation between feature sets
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 13 August 2021
#----------------------------------------

# Get list of available pairwise correlation datafiles and bind together

files <- list.files("data/corMat", 
                    full.names = TRUE, pattern = "\\.Rda", all.files = TRUE)

storage <- list()

for(f in files){
  
  tmp <- load(f)
  storage[[f]] <- tmp
}

corMats <- rbindlist(storage, use.names = TRUE)

#------------------ Preprocessing -------------------

# Add feature set labels back in



#------------------ Graphical summary ---------------


