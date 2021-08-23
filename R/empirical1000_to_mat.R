#----------------------------------------
# This script sets out to convert the
# truncated Empirical 1000 to a .mat file
# appropriate for hctsa
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 23 August 2021
#----------------------------------------

# Read in data

load("data/empirical1000.Rda")

# Parse into 3 separate list objects as per hctsa documentation here: https://hctsa-users.gitbook.io/hctsa-manual/calculating/input_files

#---------------
# timeSeriesData
#---------------

uniqueIDs <- empirical1000 %>% 
  dplyr::select(c(id)) %>% 
  distinct() %>% 
  pull(id)

timeSeriesData <- list()

for(i in 1:length(uniqueIDs)){
  
  message(paste0("Doing ID: ",i))
  
  tmp <- empirical1000 %>%
    filter(id == i) %>%
    dplyr::select(c(timepoint, value)) %>%
    arrange(timepoint) %>%
    pull(value)
  
  timeSeriesData[[i]] <- list(tmp)
}

#-------
# labels
#-------

# Get unique list of IDs

uniquelabels <- empirical1000 %>% 
  dplyr::select(c(Name)) %>% 
  distinct() %>% 
  pull(Name)

# Append into a list like how hctsa needs

labels <- list()

for(i in 1:length(uniquelabels)){
  
  labels[[i]] <- list(uniquelabels[1])
}

#---------
# keywords
#---------

# Get unique list of keywords arranged by ID

uniquekeywords <- empirical1000 %>% 
  dplyr::select(c(Name, Keywords)) %>% 
  distinct() %>% 
  pull(Keywords)

# Append into a list like how hctsa needs

keywords <- list()

for(i in 1:length(uniquekeywords)){
  
  keywords[[i]] <- list(uniquelabels[1])
}

#--------------------------
# Merge together and export
#--------------------------

writeMat("data/empirical1000.mat", timeSeriesData = timeSeriesData, labels = labels, keywords = keywords)
