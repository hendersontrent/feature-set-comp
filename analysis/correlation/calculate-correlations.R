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
  mutate(comb_id = paste0(method,"_",names)) %>%
  dplyr::select(c(comb_id, method, values))

# Convert dataframe to data.table for faster operations
# One is for pairwise matrices and one is for feature x feature correlations

normedDT <- data.table(normed)
setkey(normedDT, method) 
normedDT2 <- normed
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
