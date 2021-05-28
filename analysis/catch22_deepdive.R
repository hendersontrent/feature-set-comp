#------------------------------------
# This script sets out to produce a
# low dimension deep-dive of catch22
# using PCA, t-SNE and UMAP
#
# NOTE: setup.R must be run first
#------------------------------------

#-------------------------------------
# Author: Trent Henderson, 12 May 2021
#-------------------------------------

# Read in data

load("data/featureMatrix.Rda")

# Filter to just catch22

caught22 <- featureMatrix %>%
  filter(method == "catch22")

# Drop big dataframe from memory

rm(featureMatrix)

#-------------- Low dimension ---------------

#' Function to calculate and plot low dimension representations for each time-series problem
#' 
#' @param data the dataframe of feature calculations to use
#' @param low_dim_method the low dimension representation algorithm to use
#' @return ggplot2 graphic object containing matrix of plots in two dimensions
#' @author Trent Henderson
#' 

full_low_dim <- function(data, low_dim_method = c("PCA", "tSNE", "UMAP")){
  
  # Run calculations
  
  problems <- unique(data$problem)
  storage <- list()
  
  for(i in problems){
    
    x
    
  }
  
  
  
  # Draw plots and store output
  
  CairoPNG(paste0("output/catch22_lowdim_",low_dim_method,".png"), 1200, 1200)
  XXX
  dev.off()
  
}

#-------------- Run function ----------------

full_low_dim(data = caught22, low_dim_method = "PCA")
full_low_dim(data = caught22, low_dim_method = "tSNE")
full_low_dim(data = caught22, low_dim_method = "UMAP")
