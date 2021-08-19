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



# Compute mean maximum absolute correlation between each feature set

mean_maxabscors

# Compute min absolute correlation between each feature set

min_maxabscors

# Impute self-correlations for matrix graphic

mean_maxabscors <- bind_rows(mean_maxabscors, selfcors)
min_maxabscors <- bind_rows(min_maxabscors, selfcors)

#------------------ Graphical summary ---------------

# Mean

p <- mean_maxabscors %>%
  ggplot(aes(x = var1, y = var2, fill = maxcor)) +
  geom_tile(aes(width = 0.9, height = 0.9), stat = "identity") +
  labs(x = "Feature Set",
       y = "Feature Set") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)

# Min

p1 <- min_maxabscors %>%
  ggplot(aes(x = var1, y = var2, fill = maxcor)) +
  geom_tile(aes(width = 0.9, height = 0.9), stat = "identity") +
  labs(x = "Feature Set",
       y = "Feature Set") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)1
