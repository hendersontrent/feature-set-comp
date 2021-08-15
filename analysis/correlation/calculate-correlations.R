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

# Compute all correlations

# NOTE: Storing these OUTSIDE of the project folder as it's local on my machine. These paths
# will need to be manually changed for a different user if they intend to replicate.

return_cor_mat(normed, "catch22", "feasts", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/1.Rda")
return_cor_mat(normed, "catch22", "tsfeatures", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/2.Rda")
return_cor_mat(normed, "catch22", "Kats", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/3.Rda")
return_cor_mat(normed, "catch22", "tsfresh", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/4.Rda")
return_cor_mat(normed, "catch22", "TSFEL", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/5.Rda")
return_cor_mat(normed, "catch22", "hctsa", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/6.Rda")
return_cor_mat(normed, "feasts", "tsfeatures", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/7.Rda")
return_cor_mat(normed, "feasts", "Kats", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/8.Rda")
return_cor_mat(normed, "feasts", "tsfresh", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/9.Rda")
return_cor_mat(normed, "feasts", "TSFEL", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/10.Rda")
return_cor_mat(normed, "feasts", "hctsa", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/11.Rda")
return_cor_mat(normed, "tsfeatures", "Kats", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/12.Rda")
return_cor_mat(normed, "tsfeatures", "tsfresh", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/13.Rda")
return_cor_mat(normed, "tsfeatures", "TSFEL", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/14.Rda")
return_cor_mat(normed, "tsfeatures", "hctsa", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/15.Rda")
return_cor_mat(normed, "Kats", "tsfresh", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/16.Rda")
return_cor_mat(normed, "Kats", "TSFEL", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/17.Rda")
return_cor_mat(normed, "Kats", "hctsa", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/18.Rda")
return_cor_mat(normed, "tsfresh", "TSFEL", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/19.Rda")
return_cor_mat(normed, "tsfresh", "hctsa", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/20.Rda")
return_cor_mat(normed, "TSFEL", "hctsa", store = TRUE, store_to = "/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/21.Rda")
