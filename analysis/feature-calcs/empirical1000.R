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

#--------------------------------
# Do the others in TryCatch loops
# as errors arise otherwise
#--------------------------------

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

outs_fe <- do_features(the_set = "feasts")
outs_ts <- do_features(the_set = "tsfeatures")
outs_tsfresh <- do_features(the_set = "tsfresh")
outs_tsfel <- do_features(the_set = "tsfel")
outs_kats <- do_features(the_set = "kats")
      
# Bind together

Emp1000FeatMat <- bind_rows(outs_22, outs_fe, outs_ts, outs_tsfresh, outs_tsfel, outs_kats)

# Save as .Rda

save(Emp1000FeatMat, file = "data/Emp1000FeatMat.Rda")

# Clean up environment

rm(empirical1000, outs_22, outs_fe, outs_ts, outs_tsfresh, outs_tsfel, outs_kats, Emp1000FeatMat)
