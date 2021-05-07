#--------------------------------------
# This script sets out to webscrape
# all the univariate time-series 
# classification datasets
#
# NOTE: This script requires setup.R to
# have been run first
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 7 May 2021
#--------------------------------------

#' Function to automatically webscrape and parse Time Series Classification univariate two-class classification datasets
#' 
#' NOTE: The dictionary list used to identify and pass two-class problems only should be switched to a dynamic
#' webscrape table read to ensure it can scale as the dataset structure changes/is added to.
#' 
#' @return a list object with each of the problems as a dataframe
#' @author Trent Henderson
#' 

pullTSCprobs <- function(){
  
  message("Downloading data... This may take a long time as the file is >500MB.")
  
  # --------------- Set up dictionary -------------
  
  # Not all the datasets are two-class problems. Define dictionary from
  # website of two-class problems to filter downloaded dataset by
  # Source: http://www.timeseriesclassification.com/dataset.php
  
  twoclassprobs <- c("Yoga", "WormsTwoClass", "Wine", 
                     "Wafer", "TwoLeadECG", "ToeSegmentation2", 
                     "ToeSegmentation1", "Strawberry", "SonyAIBORobotSurface2", 
                     "SonyAIBORobotSurface1", "SharePriceIncrease", "ShapeletSim", 
                     "SemgHandGenderCh2", "SelfRegulationSCP2", "SelfRegulationSCP1", 
                     "RightWhaleCalls", "ProximalPhalanxOutlineCorrect", "PowerCons",
                     "PhalangesOutlinesCorrect", "MotorImagery", "MoteStrain", 
                     "MiddlePhalanxOutlineCorrect", "Lightning2", "ItalyPowerDemand", 
                     "HouseTwenty", "Herring", 
                     "HandOutlines", "Ham", "GunPointOldVersusYoung", 
                     "GunPointMaleVersusFemale", "GunPointAgeSpan", "GunPoint", 
                     "FreezerSmallTrain", "FreezerRegularTrain", "FordB",
                     "FordA", "ECGFiveDays", 
                     "ECG200", "Earthquakes", "DodgerLoopWeekend", 
                     "DodgerLoopGame", "DistalPhalanxOutlineCorrect", "Computers", 
                     "Coffee", "Chinatown", 
                     "BirdChicken", "BeetleFly")
  
  # --------------- Webscrape the data ------------
  
  temp <- tempfile()
  download.file("http://www.timeseriesclassification.com/Downloads/Archives/Univariate2018_arff.zip", temp, mode = "wb")
  
  # --------------- Parse into problems -----------
  
  problemStorage <- list()
  message("Parsing individual datasets...")
  
  for(i in twoclassprobs){
    
    tryCatch({
      
      path <- paste0("Univariate_arff/",i,"/")
      
      # Retrieve TRAIN and TEST files
      
      train <- foreign::read.arff(unz(temp, paste0(path,i,"_TRAIN.arff"))) %>%
        mutate(id = row_number()) %>%
        mutate(set_split = "Train")
      
      test <- foreign::read.arff(unz(temp, paste0(path,i,"_TEST.arff"))) %>%
        mutate(id = row_number()) %>%
        mutate(set_split = "Test")
      
      # Merge
      
      tmp <- bind_rows(train, test)
      
      thecols <- colnames(tmp)
      keepcols <- thecols[!thecols %in% c("target", "id", "set_split")]
      
      tmp1 <- tmp %>%
        mutate(problem = i) %>%
        tidyr::pivot_longer(cols = all_of(keepcols), names_to = "timepoint", values_to = "values") %>%
        mutate(timepoint = as.numeric(gsub(".*?([0-9]+).*", "\\1", timepoint)))
      
      problemStorage[[i]] <- tmp
      
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  problemStorage2 <- rbindlist(problemStorage, use.names = TRUE)
  return(problemStorage)
}

allProbs <- pullTSCprobs()
save(allProbs, file = "data/allProbs.Rda") # Store as download is very large
