#------------------------------------
# This script sets out to calculate
# time-series features for each
# classification problem and retain
# them for analytical use
#
# NOTE: setup.R must be run first
#------------------------------------

#------------------------------------
# Author: Trent Henderson, 7 May 2021
#------------------------------------

# Read in data

load("data/allProbs.Rda")

# Fix Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

#----------------- Calculate features -----------------

#' Function to automatically calculate all features for all datasets and feature sets
#' 
#' @param data the data containing raw time series for each problem
#' @return a dataframe object containing the computed feature matrices
#' @author Trent Henderson
#' 

calculate_feats_all_probs <- function(data){
  
  sets <- unique(data$problem)
  storage <- list()
  
  for(s in sets){
    
    tryCatch({
    
    message(paste0("Calculating features for ",s))
    
    tmp <- data %>%
      filter(problem == s)
    
    # Get accessory variables to join back in
    
    accessories <- tmp %>%
      group_by(id, target, set_split, problem) %>%
      summarise(counter = n()) %>%
      ungroup() %>%
      dplyr::select(-c(counter))
    
    # Calculate features
    
    feats <- theft::calculate_features(data = tmp, id_var = "id", time_var = "timepoint", values_var = "values", feature_set = "all", tsfresh_cleanup == FALSE)
    feats1 <- calc_tsfresh(data = tmp, column_id = "id", column_sort = "timepoint", cleanup = "No")
    feats2 <- calc_tsfel(data = tmp)
    
    # Join in accessory variables and store
    
    feats3 <- bind_rows(feats, feats1, feats2) %>%
      left_join(accessories, by = c("id" = "id"))
    
    storage[[s]] <- feats3
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  outputData <- rbindlist(storage, use.names = TRUE)
  return(outputData)
}

featureMatrix <- calculate_feats_all_probs(data = allProbs)

# Save as .Rda

save(featureMatrix, file = "data/featureMatrix.Rda")
