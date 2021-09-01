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

source("R/process_hctsa_csv.R")
hctsa <- process_hctsa_csv() 

# Merge together

fullFeatMat <- bind_rows(Emp1000FeatMat, hctsa)

# Clean up environment as files are big

rm(Emp1000FeatMat, hctsa)

# Get list of time series that have 0 NAs across all features after removing features with >10% NAs

source("R/utility_functions.R")

good_ids <- remove_problematic_datasets(fullFeatMat)

# Remove features with >10% NAs

good_feats <- remove_problematic_features(fullFeatMat)

# Cycle through sets and retain only good features then drop problematic time series

thesets <- unique(fullFeatMat$method)
storage <- list()

for(i in thesets){
  
  tmp <- fullFeatMat %>%
    filter(method == i)
  
  tmp2 <- good_feats %>%
    filter(feature_set == i)
  
  tmp3 <- tmp %>%
    filter(names %in% unique(tmp2$names))
  
  storage[[i]] <- tmp3
}

fullFeatMat2 <- rbindlist(storage, use.names = TRUE)

fullFeatMat3 <- fullFeatMat2 %>%
  filter(id %in% good_ids) %>%
  mutate(comb_id = paste0(method,"_",names))

rm(fullFeatMat, fullFeatMat2, good_feats, good_ids, i, tmp, 
   tmp2, tmp3, thesets, storage)

# Normalise feature vectors

fullFeatMat4 <- fullFeatMat3 %>%
  group_by(comb_id) %>%
  mutate(values = normalise_feature_vector(values, method = "z-score")) %>%
  ungroup()

rm(fullFeatMat3)

#-------------- Compute correlations ----------------

# Convert dataframe to data.table for faster operations and set up keys
# One is for pairwise matrices and one is for feature x feature correlations

normedDT <- data.table(fullFeatMat4)
setkey(normedDT, method) 
normedDT2 <- data.table(fullFeatMat4)
setkey(normedDT2, comb_id)
rm(normed)

# Compute all correlations

return_cor_mat(normedDT, normedDT2, "catch22", "feasts", "spearman", save_to = "data/corMats/1.Rda")
return_cor_mat(normedDT, normedDT2, "catch22", "tsfeatures", "spearman", save_to = "data/corMats/2.Rda")
return_cor_mat(normedDT, normedDT2, "catch22", "Kats", "spearman", save_to = "data/corMats/3.Rda")
return_cor_mat(normedDT, normedDT2, "catch22", "tsfresh", "spearman", save_to = "data/corMats/4.Rda")
return_cor_mat(normedDT, normedDT2, "catch22", "TSFEL", "spearman", save_to = "data/corMats/5.Rda")
return_cor_mat(normedDT, normedDT2, "catch22", "hctsa", "spearman", save_to = "data/corMats/6.Rda")
return_cor_mat(normedDT, normedDT2, "feasts", "tsfeatures", "spearman", save_to = "data/corMats/7.Rda")
return_cor_mat(normedDT, normedDT2, "feasts", "Kats", "spearman", save_to = "data/corMats/8.Rda")
return_cor_mat(normedDT, normedDT2, "feasts", "tsfresh", "spearman", save_to = "data/corMats/9.Rda")
return_cor_mat(normedDT, normedDT2, "feasts", "TSFEL", "spearman", save_to = "data/corMats/10.Rda")
return_cor_mat(normedDT, normedDT2, "feasts", "hctsa", "spearman", save_to = "data/corMats/11.Rda")
return_cor_mat(normedDT, normedDT2, "tsfeatures", "Kats", "spearman", save_to = "data/corMats/12.Rda")
return_cor_mat(normedDT, normedDT2, "tsfeatures", "tsfresh", "spearman", save_to = "data/corMats/13.Rda")
return_cor_mat(normedDT, normedDT2, "tsfeatures", "TSFEL", "spearman", save_to = "data/corMats/14.Rda")
return_cor_mat(normedDT, normedDT2, "tsfeatures", "hctsa", "spearman", save_to = "data/corMats/15.Rda")
return_cor_mat(normedDT, normedDT2, "Kats", "tsfresh", "spearman", save_to = "data/corMats/16.Rda")
return_cor_mat(normedDT, normedDT2, "Kats", "TSFEL", "spearman", save_to = "data/corMats/17.Rda")
return_cor_mat(normedDT, normedDT2, "Kats", "hctsa", "spearman", save_to = "data/corMats/18.Rda")
return_cor_mat(normedDT, normedDT2, "tsfresh", "TSFEL", "spearman", save_to = "data/corMats/19.Rda")
return_cor_mat(normedDT, normedDT2, "tsfresh", "hctsa", "spearman", save_to = "data/corMats/20.Rda")
return_cor_mat(normedDT, normedDT2, "TSFEL", "hctsa", "spearman", save_to = "data/corMats/21.Rda")
