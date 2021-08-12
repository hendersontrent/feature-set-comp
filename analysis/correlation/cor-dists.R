#------------------------------------
# This script sets out to calculate
# the distribution of feature
# correlations between sets
#
# NOTE: setup.R must be run first
#------------------------------------

#----------------------------------------
# Author: Trent Henderson, 12 August 2021
#----------------------------------------

# Load feature matrix

load("data/Emp1000FeatMat.Rda")

# Filter data to unique entries

tmp <- Emp1000FeatMat %>%
  dplyr::select(c(id, names, method, values)) %>%
  distinct() %>%
  drop_na()

#-------------- Compute correlations ----------------

# Compute all pairwise correlations between a feature and every other feature
# included in all other sets

storage <- list()
the_sets <- unique(tmp$method)

for(i in the_sets){
  
  tmp_i <- tmp %>%
    filter(method == i)
  
  tmp_not_i <- tmp %>%
    filter(method != i)
  
}

#-------------- Generate data vis -------------------

# Plot distribution of correlations



# SPlot summary matrix of maximum correlations between feature sets


