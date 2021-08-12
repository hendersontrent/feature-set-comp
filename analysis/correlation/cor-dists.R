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

# Filter data to unique entries and normalise

tmp <- Emp1000FeatMat %>%
  dplyr::select(c(id, names, method, values)) %>%
  distinct() %>%
  drop_na()

# Retain only datasets on which all feature sets successfully computed

source("R/utility_functions.R")

good_datasets <- get_consistent_datasets()

fullFeatMat_filt <- fullFeatMat %>%
  filter(id %in% good_datasets)

normed <- normalise_feature_frame(fullFeatMat_filt, names_var = "names", values_var = "values",
                                  method = "z-score")

# Clean up environment

rm(fullFeatMat, fullFeatMat_filt)

#-------------- Compute correlations ----------------

# Compute all pairwise correlations between a feature and every other feature
# included in all other sets

storage <- list()
the_sets <- unique(normed$method)

for(i in the_sets){
  
  tmp_i <- normed %>%
    filter(method == i) %>%
    drop_na()
  
  tmp_not_i <- normed %>%
    filter(method != i) %>%
    drop_na()
  
  # Loop through each feature in set i and get correlations
  
  the_feats <- unique(tmp_i$names)
  storage2 <- list()
  
  for(f in the_feats){
    
    val <- tmp_i %>%
      filter(names == f) %>%
      dplyr::select(values) %>%
      pull()
    
    other_vals <- tmp_not_i %>%
      mutate(comb_id = paste0(method,"_",names)) %>% # Preps for duplicate names across sets
      dplyr::select(c(id, comb_id, values)) %>%
      pivot_wider(id_cols = id, names_from = comb_id, values_from = values)
    
    ncols <- ncol(other_vals)
    corMat <- data.frame()
    
    for(n in 2:ncols){
      
      thename <- colnames(other_vals[,n])
      
      corMat <- corMat %>%
        mutate(thename = cor(val, other_vals[,n]))
    }
    
    storage2[[f]] <- mycors
  }
  
  # Store output
  
  storage[[i]] <- storage2
  
}

#-------------- Generate data vis -------------------

# Plot distribution of correlations



# Plot summary matrix of maximum correlations between feature sets


