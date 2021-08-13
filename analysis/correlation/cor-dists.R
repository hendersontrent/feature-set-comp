#------------------------------------
# This script sets out to calculate
# the distribution of feature
# correlations between sets
#
# NOTE: setup.R must be run first
#------------------------------------

#----------------------------------------
# Author: Trent Henderson, 12 August 2021
#----------------------------------------

# Load feature matrix

load("data/Emp1000FeatMat.Rda")

# Load hctsa results

source("webscraping/pull_hctsa_results.R")
hctsa <- pull_hctsa_results() 

# Merge together and remove erroneous duplicates

fullFeatMat <- bind_rows(Emp1000FeatMat, hctsa) %>%
  dplyr::select(c(id, names, method, values)) %>%
  distinct()

# Retain only datasets on which all feature sets successfully computed

source("R/utility_functions.R")

good_datasets <- get_consistent_datasets()

fullFeatMat_filt <- fullFeatMat %>%
  filter(id %in% good_datasets)

# Filter to only datasets that every individual feature computed on

good_ind_datasets <- get_consistent_datasets_feats(fullFeatMat_filt)

fullFeatMat_filt2 <- fullFeatMat_filt %>%
  filter(id %in% good_ind_datasets)

# Normalise features

normed <- normalise_feature_frame(fullFeatMat_filt2, names_var = "names", values_var = "values",
                                  method = "z-score")

# Clean up environment

rm(Emp1000FeatMat, hctsa, fullFeatMat, fullFeatMat_filt, fullFeatMat_filt2)

#-------------- Compute correlations ----------------

# Preps for duplicate feature names across sets

normed <- normed %>%
  mutate(comb_id = paste0(method,"_",names))

# Get a matrix of pairwise combinations of features in a function to avoid
# cluttering environment

#' Function to get results and merge together
#' 
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

retrieve_results <- function(){
  
  mat1 <- return_cor_mat(normed, "catch22", "feasts")
  mat2 <- return_cor_mat(normed, "catch22", "tsfeatures")
  mat3 <- return_cor_mat(normed, "catch22", "Kats")
  mat4 <- return_cor_mat(normed, "catch22", "tsfresh")
  mat6 <- return_cor_mat(normed, "catch22", "TSFEL")
  mat7 <- return_cor_mat(normed, "catch22", "hctsa")
  mat8 <- return_cor_mat(normed, "feasts", "tsfeatures")
  mat9 <- return_cor_mat(normed, "feasts", "Kats")
  mat10 <- return_cor_mat(normed, "feasts", "tsfresh")
  mat11 <- return_cor_mat(normed, "feasts", "TSFEL")
  mat12 <- return_cor_mat(normed, "feasts", "hctsa")
  mat13 <- return_cor_mat(normed, "tsfeatures", "Kats")
  mat14 <- return_cor_mat(normed, "tsfeatures", "tsfresh")
  mat15 <- return_cor_mat(normed, "tsfeatures", "TSFEL")
  mat16 <- return_cor_mat(normed, "tsfeatures", "hctsa")
  mat17 <- return_cor_mat(normed, "Kats", "tsfresh")
  mat18 <- return_cor_mat(normed, "Kats", "TSFEL")
  mat19 <- return_cor_mat(normed, "Kats", "hctsa")
  mat20 <- return_cor_mat(normed, "tsfresh", "TSFEL")
  mat21 <- return_cor_mat(normed, "tsfresh", "hctsa")
  mat22 <- return_cor_mat(normed, "TSFEL", "hctsa")
  
  myResults <- bind_rows(mat1, mat2, mat3, mat4, mat5, mat6,
                         mat7, mat8, mat9, mat10, mat11, mat12,
                         mat13, mat14, mat15, mat16, mat17, mat18,
                         mat19, mat20, mat21, mat22)
  
  return(myResults)
}

outputCor <- retrieve_results()

#-------------- Generate data vis -------------------

# Plot distribution of correlations



# Plot summary matrix of maximum correlations between feature sets


