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

timeSeriesData <- empirical1000 %>%
  dplyr::select(c(id, timepoint, value)) %>%
  pivot_wider(id_cols = "id", names_from = "timepoint", values_from = "value") %>%
  dplyr::select(-c(id))

timeSeriesData <- as.matrix(timeSeriesData)

timeSeriesData[is.na(timeSeriesData)] <- ""

#-------
# labels
#-------

# Get unique list of IDs

labels <- c(empirical1000 %>% 
  dplyr::select(c(Name)) %>% 
  distinct() %>% 
  pull(Name))

#---------
# keywords
#---------

# Get unique list of keywords arranged by ID

keywords <- c(empirical1000 %>% 
  dplyr::select(c(Name, Keywords)) %>% 
  distinct() %>% 
  pull(Keywords))

#--------------------------
# Merge together and export
#--------------------------

writeMat("data/empirical1000.mat", timeSeriesData = timeSeriesData, labels = labels, keywords = keywords)
