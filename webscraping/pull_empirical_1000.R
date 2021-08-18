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
#' @return a dataframe with the datasets organised in a tidy format
#' @author Trent Henderson
#' 

pull_empirical_1000 <- function(){
  
  message("Downloading data... This may take a long time as the file is large.")
  
  # Data
  
  temp <- tempfile()
  download.file("https://ndownloader.figshare.com/files/24950795",temp)
  ts <- read.csv(temp, header = FALSE)
  
  ts <- ts %>%
    mutate(id = dplyr::row_number()) %>%
    pivot_longer(!id, names_to = "timepoint", values_to = "value") %>%
    mutate(timepoint = as.numeric(gsub("V", "\\1", timepoint))) %>%
    drop_na()
  
  # Information file
  
  temp1 <- tempfile()
  download.file("https://ndownloader.figshare.com/files/24950798",temp1)
  ts_info <- read.csv(temp1, header = TRUE)
  
  # Merge
  
  main <- ts %>%
    dplyr::left_join(ts_info, by = c("id" = "ID"))
  
  return(main)
}

# Run function and save output

empirical1000 <- pull_empirical_1000()
save(empirical1000, file = "data/empirical1000.Rda")
