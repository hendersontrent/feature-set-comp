#------------------------------------
# This script sets out to calculate
# dimension reduction properties on
# the extracted feature matrices for
# the Empirical 1000
#
# NOTE: setup.R must be run first
#------------------------------------

#---------------------------------------
# Author: Trent Henderson, 9 August 2021
#---------------------------------------

# Load feature matrix

load("data/Emp1000FeatMat.Rda")

# Load hctsa results

source("webscraping/pull_hctsa_results.R")
hctsa <- pull_hctsa_results() 

# Merge together

fullFeatMat <- bind_rows(Emp1000FeatMat, hctsa)

# Clean up environment as files are big

rm(Emp1000FeatMat, hctsa)

#-------------- Preliminary calculations----------------

#------------------------
# Retain only datasets on 
# which all feature sets 
# successfully completed
#------------------------

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

good_datasets <- get_consistent_datasets()

fullFeatMat_filt <- fullFeatMat %>%
  filter(id %in% good_datasets)

# Clean up environment

rm(fullFeatMat)

#---------------------------
# Total number of features 
# by feature set and dataset
#---------------------------

num_feats <- fullFeatMat_filt %>%
  dplyr::select(c(id, names, method)) %>%
  distinct() %>%
  group_by(id, names, method) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  dplyr::select(c(names, method)) %>%
  distinct()

#-------------- Do PCA for each feature set ------------

#' Function to produce PCA on each Dataset x Feature matrix and bind results
#' 
#' @param dataset the Dataset x Feature matrix dataframe
#' @return a dataframe containing PC results
#' @author Trent Henderson
#' 

do_pca_summary <- function(dataset){
  
  the_sets <- unique(dataset$method)
  storage <- list()
  
  # Iterate through each set
  
  for(i in the_sets){
    
    tryCatch({
    
    # Filter to set
    
    tmp <- dataset %>%
      filter(method == i) %>%
      dplyr::select(c(id, names, values)) %>%
      distinct() %>%
      drop_na()
    
    # Remove features that didn't calculate on all datasets
    
    feat_list <- num_feats %>%
      filter(method == i) %>%
      dplyr::select(c(names)) %>%
      pull()
    
    tmp <- tmp %>%
      filter(names %in% feat_list)
    
    # Normalise features
    
    tmp <- tmp %>%
      group_by(names) %>%
      mutate(values = normalise_feature_vector(values, method = "z-score")) %>%
      ungroup() %>%
      drop_na()
    
    # Widen the matrix
    
    dat <- tmp %>%
      pivot_wider(id_cols = id, names_from = names, values_from = values) %>%
      tibble::column_to_rownames(var = "id") %>%
      drop_na()
    
    # Compute PCA
    
    fits <- dat %>%
      prcomp(scale = FALSE)
    
    eigenvalues <- fits %>%
      tidy(matrix = "eigenvalues") %>%
      dplyr::select(c(PC, percent)) %>%
      mutate(feature_set = i)
    
    # Store output
    
    storage[[i]] <- eigenvalues
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  pc_results <- rbindlist(storage, use.names = TRUE)
  return(pc_results)
}

# Run function and compute cumulative sum for later

pca_results <- do_pca_summary(fullFeatMat_filt) %>%
  group_by(feature_set) %>%
  arrange(PC) %>%
  mutate(cs = cumsum(percent)) %>%
  ungroup()

#-------------- Produce summary graphic ----------------

# Scaled by number of features

p <- pca_results %>%
  group_by(feature_set) %>%
  mutate(PC = PC/max(PC)) %>%
  ungroup() %>%
  ggplot(aes(x = (PC*100), y = (percent*100), colour = feature_set)) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "% of Principal Components",
       y = "Variance Explained (%)",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)

p1 <- pca_results %>%
  group_by(feature_set) %>%
  mutate(PC = PC/max(PC)) %>%
  ungroup() %>%
  ggplot(aes(x = (PC*100), y = (cs*100), colour = feature_set)) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "% of Principal Components",
       y = "Cumulative Variance Explained (%)",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p1)

# Save plots

ggsave("output/pca-scaled.png", p)
ggsave("output/pca-cumsum-scaled.png", p1)
