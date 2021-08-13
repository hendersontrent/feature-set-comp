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
  
  feats <- unique(tmp_i$names)
  storage2 <- list()
  
  for(f in feats){
    
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


