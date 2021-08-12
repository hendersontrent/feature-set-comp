#---------------------------------------
# This script sets out to webscrape the
# computed hctsa feature matrix from
# Ben's figshare
#---------------------------------------

#----------------------------------------
# Author: Trent Henderson, 12 August 2021
#----------------------------------------

#' Function to grab hctsa outputs
#' 
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

pull_hctsa_results <- function(){
  
  message("Downloading data... This may take a long time.")
  
  # --------------- Webscrape the data ------------
  
  #--------------------
  # PART I: DATA MATRIX
  #--------------------
  
  temp <- tempfile()
  download.file("https://ndownloader.figshare.com/files/29061867", temp, mode = "wb")
  d <- readr::read_csv(temp, col_names = FALSE)
  
  #-------------
  # PART II: IDs
  #-------------
  
  
  
  #-------------------
  # PART III: FEATURES
  #-------------------
  
  
  
  # Merge all together
  
  
  
}
