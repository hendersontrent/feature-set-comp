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

normedDT <- data.table(normedDT)
setkey(normedDT, comb_id)
rm(normed) # Free up memory

# Compute all correlations

return_cor_mat(normedDT, "catch22", "feasts", store = TRUE, store_to = "data/1.Rda")
return_cor_mat(normedDT, "catch22", "tsfeatures", store = TRUE, store_to = "data/2.Rda")
return_cor_mat(normedDT, "catch22", "Kats", store = TRUE, store_to = "data/3.Rda")
return_cor_mat(normedDT, "catch22", "tsfresh", store = TRUE, store_to = "data/4.Rda")
return_cor_mat(normedDT, "catch22", "TSFEL", store = TRUE, store_to = "data/5.Rda")
return_cor_mat(normedDT, "catch22", "hctsa", store = TRUE, store_to = "data/6.Rda")
return_cor_mat(normedDT, "feasts", "tsfeatures", store = TRUE, store_to = "data/7.Rda")
return_cor_mat(normedDT, "feasts", "Kats", store = TRUE, store_to = "data/8.Rda")
return_cor_mat(normedDT, "feasts", "tsfresh", store = TRUE, store_to = "data/9.Rda")
return_cor_mat(normedDT, "feasts", "TSFEL", store = TRUE, store_to = "data/10.Rda")
return_cor_mat(normedDT, "feasts", "hctsa", store = TRUE, store_to = "data/11.Rda")
return_cor_mat(normedDT, "tsfeatures", "Kats", store = TRUE, store_to = "data/12.Rda")
return_cor_mat(normedDT, "tsfeatures", "tsfresh", store = TRUE, store_to = "data/13.Rda")
return_cor_mat(normedDT, "tsfeatures", "TSFEL", store = TRUE, store_to = "data/14.Rda")
return_cor_mat(normedDT, "tsfeatures", "hctsa", store = TRUE, store_to = "data/15.Rda")
return_cor_mat(normedDT, "Kats", "tsfresh", store = TRUE, store_to = "data/16.Rda")
return_cor_mat(normedDT, "Kats", "TSFEL", store = TRUE, store_to = "data/17.Rda")
return_cor_mat(normedDT, "Kats", "hctsa", store = TRUE, store_to = "data/18.Rda")
return_cor_mat(normedDT, "tsfresh", "TSFEL", store = TRUE, store_to = "data/19.Rda")
return_cor_mat(normedDT, "tsfresh", "hctsa", store = TRUE, store_to = "data/20.Rda")
return_cor_mat(normedDT, "TSFEL", "hctsa", store = TRUE, store_to = "data/21.Rda")
