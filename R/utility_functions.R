#----------------------------------------
# This script sets out to define a set of
# utility functions for use in the project
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 12 August 2021
#----------------------------------------

#---------------- Wrangling helpers ----------------

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

#---------------- Correlation helpers --------------

#' Function to produce pairwise matrices for each feature in a set and each other set
#' 
#' @param dataset the dataset containing normalised feature matrices
#' @param x the first set of interest
#' @param y the second set of interest
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

make_pairwise_matrix <- function(dataset, x, y){
  
  vector1 <- dataset %>%
    filter(method == x) %>%
    dplyr::select(c(comb_id)) %>%
    pull()
  
  vector2 <- dataset %>%
    filter(method == y) %>%
    dplyr::select(c(comb_id)) %>%
    pull()
  
  myMat <- expand.grid(x = as.character(vector1), y = as.character(vector2))
  return(myMat)
}

#' Function to filter dataframe and compute correlation between two vectors
#' 
#' @param dataset the dataset containing normalised feature matrices
#' @param x the first feature to filter by
#' @param y the second feature to filter by
#' @return a numeric value of the correlation coefficient
#' @author Trent Henderson
#' 

compute_pairwise_cor <- function(dataset, x, y){
  
  vector1 <- dataset %>%
    filter(comb_id == x) %>%
    dplyr::select(c(values)) %>%
    pull()
  
  vector2 <- dataset %>%
    filter(comb_id == y) %>%
    dplyr::select(c(values)) %>%
    pull()
  
  the_cor <- cor(vector1, vector2)
  return(the_cor)
}

#' Function to produce a pairwise combination and impute correlations
#' 
#' @param dataset the dataset containing normalised feature matrices
#' @param x the first set of interest
#' @param y the second set of interest
#' @author Trent Henderson
#' 

return_cor_mat <- function(dataset, x, y){
  
  # Make matrix
  
  mat1 <- make_pairwise_matrix(dataset = normed, x = x, y = y) %>%
    distinct()
  
  # Compute correlation for each entry
  
  storage <- list()
  
  for(i in 1:nrow(mat1)){
    
    tryCatch({
    
    message(paste0("Computing correlation index: ",i))
    
    x1 <- as.character(mat1[i,1])
    y1 <- as.character(mat1[i,2])
    
    the_cor <- compute_pairwise_cor(dataset = dataset, x = x1, y = y1)
    
    corDat <- data.frame(x = x1,
                         y = y1,
                         pearson = the_cor)
    
    storage[[i]] <- corDat
    
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  corMat <- rbindlist(storage, use.names = TRUE)
  return(corMat)
}
