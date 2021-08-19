#------------------------------------
# This script sets out to calculate
# time-series features for each
# Empirical 1000 dataset and retain
# them for analytical use
#
# NOTE: setup.R must be run first
#------------------------------------

#---------------------------------------
# Author: Trent Henderson, 9 August 2021
#---------------------------------------

# Read in data

load("data/empirical1000.Rda")

# Fix Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

#----------------- Calculate features -----------------

outs_22 <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "catch22")
outs_feasts <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "feasts")
outs_tsfeatures <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "tsfeatures")
outs_kats <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "kats")
outs_tsfresh <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "tsfresh", tsfresh_cleanup = FALSE)

#---------------------------------
# Do TSFEL in a TryCatch 
# loop as errors arise otherwise and 
# theft doesn't have a solution yet
#---------------------------------

#' Function to loop through unique IDs and compute features
#' 
#' @param the_set the feature set to calculate
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

do_features <- function(the_set){
  
  theids <- unique(empirical1000$id)
  storage <- list()
  
  for(i in theids){
    
    message(paste0("Computing features for ", i))
    
    tryCatch({
      
      tmp <- empirical1000 %>%
        filter(id == i)
      
      myOutput <- calculate_features(tmp, id_var = "id", time_var = "timepoint", 
                                     values_var = "value", group_var = "Keywords", 
                                     feature_set = the_set, 
                                     tsfresh_cleanup = FALSE)
      
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    storage[[i]] <- myOutput
  }
  
  outputData <- rbindlist(storage, use.names = TRUE)
  return(outputData)
}

outs_tsfel <- do_features(the_set = "tsfel")

# Bind all together

Emp1000FeatMat <- bind_rows(outs_22, outs_feasts, outs_tsfeatures,
                            outs_kats, outs_tsfresh, outs_tsfel)

# Save

save(Emp1000FeatMat, file = "data/Emp1000FeatMat.Rda")
