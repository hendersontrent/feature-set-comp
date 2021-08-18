#--------------------------------------
# This script sets out to produce
# a matrix visualisation of the max abs
# correlation between feature sets
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 13 August 2021
#----------------------------------------

# Get list of available pairwise correlation datafiles and bind together

files <- list.files("data/corMat", 
                    full.names = TRUE, pattern = "\\.Rda", all.files = TRUE)

storage <- list()

for(f in files){
  
  tmp <- load(f)
  storage[[f]] <- tmp
}

corMats <- rbindlist(storage, use.names = TRUE)

#------------------ Calculations --------------------

# Add feature set labels back in



# Compute maximum absolute correlation between each feature set

maxabscors

# Impute self-correlations for matrix graphic



maxabscors <- bind_rows(maxabscors, selfcors)

#------------------ Graphical summary ---------------

p <- maxabscors %>%
  ggplot(aes(x = var1, y = var2, fill = maxcor)) +
  geom_tile(aes(width = 0.9, height = 0.9), stat = "identity") +
  labs(x = "Feature Set",
       y = "Feature Set") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)
