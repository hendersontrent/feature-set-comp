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
    
    message(paste0("Doing feature: ", f))
    
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

#' Function to produce unique pairwise combinations of two vectors
#' Taken from https://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
#' 
#' @param dataset the dataset containing normalised feature matrices
#' @param x the first set of interest
#' @param y the second set of interest
#' @param include.equals Boolean of whether to include equal entries for x and y. Defaults to FALSE
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

expand.grid.unique <- function(x, y, include.equals = FALSE) {
  
  x <- unique(x)
  
  y <- unique(y)
  
  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  
  do.call(rbind, lapply(seq_along(x), g))
}

#' Function to produce pairwise matrices for each feature in a set and each other set
#' 
#' @param dataset the dataset containing normalised feature matrices
#' @param x the first set of interest
#' @param y the second set of interest
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

make_pairwise_matrix <- function(dataset, x, y){
  
  x1 <- dataset[J(x)]
  x1 <- x1$comb_id
  y1 <- dataset[J(y)]
  y1 <- y1$comb_id
  
  myMat <- expand.grid.unique(x = as.character(x1), y = as.character(y1), include.equals = FALSE)
  return(myMat)
}

#' Function to produce a pairwise combination and impute correlations
#' 
#' @param dataset the dataset containing normalised feature matrices
#' @param x the first set of interest
#' @param y the second set of interest
#' @param cor_type the type of correlation to compute
#' @param store Boolean whether to save the correlation results to drive
#' @param store_to filepath of where to save the file to if store = TRUE
#' @author Trent Henderson
#' 

return_cor <- function(dataset, x, y, cor_type = c("pearson", "spearman")){
  
  x1 <- dataset[J(x)]
  x1 <- x1$values
  y1 <- dataset[J(y)]
  y1 <- y1$values
  
  the_cor <- cor(x1, y1, method = cor_type, use = "pairwise.complete.obs")
  return(the_cor)
}

#' Function to drive the computations and save outputs
#' 
#' @param dataset1 the data.table containing feature set indexed data
#' @param dataset2 the data.table containing individual feature indexed data
#' @param x the first set of interest
#' @param y the second set of interest
#' @param my_cor the type of correlation to compute
#' @param save_to the filepath to save the output to
#' @author Trent Henderson
#' 

return_cor_mat <- function(dataset1, dataset2, x, y, my_cor = c("pearson", "spearman"), save_to = NULL){
  
  mat1 <- as.data.frame(make_pairwise_matrix(dataset = dataset1, x = x, y = y))
  
  corMat <- mat1 %>%
    group_by(V1, V2) %>%
    summarise(correlation = return_cor(dataset = dataset2, x = V1, y = V2, cor_type = my_cor)) %>%
    ungroup()
  
  save(corMat, file = save_to)
}
