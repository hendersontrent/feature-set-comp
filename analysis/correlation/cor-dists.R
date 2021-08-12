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

normed <- normalise_feature_frame(tmp, names_var = "names", values_var = "values",
                                  method = "z-score")

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
    filter(id %in% unique(tmp_i$id)) %>% # Filter to just datasets that computed for i
    drop_na()
  
  catch22 <- subset(tmp_not_i, method == "catch22")
  feasts <- subset(tmp_not_i, method == "feasts")
  tsfeatures <- subset(tmp_not_i, method == "tsfeatures")
  Kats <- subset(tmp_not_i, method == "Kats")
  tsfresh <- subset(tmp_not_i, method == "tsfresh")
  TSFEL <- subset(tmp_not_i, method == "TSFEL")
  hctsa <- subset(tmp_not_i, method == "hctsa")
  
  catch22 <- catch22$id
  feasts <- feasts$id
  tsfeatures <- tsfeatures$id
  Kats <- Kats$id
  tsfresh <- tsfresh$id
  TSFEL <- TSFEL$id
  hctsa <- hctsa$id
  
  tst <- c(unique(catch22), unique(feasts), unique(tsfeatures), 
           unique(Kats), unique(tsfresh), unique(TSFEL), unique(hctsa))
  
  tst <- tst[duplicated(tst)]
  
  tmp_i <- tmp_i %>%
    filter(id %in% unique(tst)) # Filter to just datasets that computed for not i
  
  tmp_not_i <- tmp_not_i %>%
    filter(id %in% unique(tst))
  
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


