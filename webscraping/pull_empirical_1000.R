#--------------------------------------
# This script sets out to webscrape
# the Empirical 1000 dataset
#
# NOTE: This script requires setup.R to
# have been run first
#--------------------------------------

#---------------------------------------
# Author: Trent Henderson, 9 August 2021
#---------------------------------------

#' Function to automatically webscrape and process the Empirical 1000 datasets
#' 
#' 
#' @return a list object with each of the problems as a dataframe
#' @author Trent Henderson
#' 

pull_empirical_1000 <- function(){
  
  message("Downloading data... This may take a long time as the file is large.")
  
}

# Run function and save output

empirical1000 <- pull_empirical_1000()
save(empirical1000, file = "data/empirical1000.Rda")
