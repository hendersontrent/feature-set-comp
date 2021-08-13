#----------------------------------------
# This script sets out to define a set of
# utility functions for use in the project
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 12 August 2021
#----------------------------------------

#' Function to find datasets that successfully computed
#' 
#' @param the_set the feature set to focus on
#' @return an object of class vector
#' @author Trent Henderson
#' 

get_computed_ids <- function(the_set){
  
  tmp <- fullFeatMat %>%
    filter(method == the_set) %>%
    drop_na() %>%
    dplyr::select(c(id)) %>%
    distinct() %>%
    pull()
  
  return(tmp)
}

#' Function to retain only the consistent time series across feature sets
#' 
#' @return an object of class vector
#' @author Trent Henderson
#' 

get_consistent_datasets <- function(){
  
  catch22 <- get_computed_ids(the_set = "catch22")
  feasts <- get_computed_ids(the_set = "feasts")
  tsfeatures <- get_computed_ids(the_set = "tsfeatures")
  Kats <- get_computed_ids(the_set = "Kats")
  tsfresh <- get_computed_ids(the_set = "tsfresh")
  TSFEL <- get_computed_ids(the_set = "TSFEL")
  hctsa <- get_computed_ids(the_set = "hctsa")
  
  tst <- c(catch22, feasts, tsfeatures, Kats, tsfresh, TSFEL, hctsa)
  
  tst <- intersect(catch22, feasts)
  tst <- intersect(tst, tsfeatures)
  tst <- intersect(tst, Kats)
  tst <- intersect(tst, tsfresh)
  tst <- intersect(tst, TSFEL)
  tst <- intersect(tst, hctsa)
  
  return(tst)
}

#' Function to retain only the consistent time series across individual features
#' 
#' @param dataset the dataset containing time series feature data
#' @return an object of class vector
#' @author Trent Henderson
#' 

get_consistent_datasets_feats <- function(dataset){
  
  message("Cycling through individual features. This will take a while.")
  
  tmp <- dataset %>%
    mutate(comb_id = paste0(method,"_",names)) # Preps for duplicate names across sets
  
  feats <- unique(tmp$comb_id)
  storage <- list()
  
  for(f in feats){
    
    tmp2 <- tmp %>%
      filter(comb_id == f) %>%
      dplyr::select(c(id)) %>%
      pull()
    
    storage[[f]] <- tmp2
  }
  
  for(n in 2:length(storage)){
    
    if(n == 2){
      the_list <- unlist(intersect(unlist(storage[n]), unlist(storage[n-1])))
    } else{
      the_list <- unlist(intersect(the_list, unlist(storage[n])))
    }
  }
  
  return(the_list)
}
