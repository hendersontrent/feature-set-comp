#----------------------------------------------
# This script sets out to produce
# classifiers for each problem and
# feature set
#
# NOTE: setup.R and calculate_feats_all_probs.R 
# must be run first
#----------------------------------------------

#------------------------------------
# Author: Trent Henderson, 7 May 2021
#------------------------------------

# Read in feature calculations

load("data/featureMatrix.Rda")

# Fix Python environment and load classification function

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)
reticulate::source_python("analysis/classifier.py")

#---------------- Fit classifiers ----------------

#' Function to automatically run all classifiers by feature set and problem
#' 
#' @param data the dataframe containing feature matrices and groups to use
#' @return a dataframe with class-balanced classification accuracy by problem and feature set
#' @author Trent Henderson
#' 

run_all_classifiers <- function(data){
  
  # Widen data
  
  x
  
  # Fit classifier
  
  theMethods <- unique(data$method)
  theProblems <- unique(data$problem)
  storage <- list()
  
  for(i in theMethods){
    storage1 <- list()
    
    for(j in theProblems){
      
      try({ 
        
        message(paste0("Computing model for: i = ",i," & j = ",j,". This will take a while..."))
        
        data2 <- data %>%
          filter(method == i) %>%
          filter(problem == j) %>%
          drop_na() %>%
          mutate(group_binary = as.factor(ifelse(group == "Control",0,1)))
        
        # Separate predictor design matrix and response vector for conversion to NumPy/Pandas
        # and train-test splits
        
        train <- data2 %>%
          filter(set_split == "Train")
        
        test <- data2 %>%
          filter(set_split == "Test")
        
        outputData <- fit_classifier(X_train = X_train, y_train = y_train, 
                              X_test = X_test, y_test = y_test)
        
        storage1[[j]] <- outputData
       })
     }
    storage[[i]] <- storage1
    }
  
  # Unlist and return results
  
  unnested1 <- do.call(rbind, storage[[1]])
  unnested2 <- do.call(rbind, storage[[2]])
  unnested3 <- do.call(rbind, storage[[3]])
  unnested4 <- do.call(rbind, storage[[4]])
  unnested5 <- do.call(rbind, storage[[5]])
  classifierOutputs <- bind_rows(unnested1, unnested2, unnested3, unnested4, unnested5)
  
}
