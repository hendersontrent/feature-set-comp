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

files <- list.files("data/corMats", 
                    full.names = TRUE, pattern = "\\.Rda", all.files = TRUE)

storage <- list()

for(f in files){
  
  load(f)
  storage[[f]] <- corMat
}

corMats <- rbindlist(storage, use.names = TRUE)

#------------------ Calculations --------------------

# Get feature set labels

corMats <- corMats %>%
  mutate(feature_set_source = gsub("_.*", "\\1", V1),
         feature_set_target = gsub("_.*", "\\1", V2))

# Compute mean maximum absolute correlation between each feature set

mean_maxabscors <- corMats

# Compute min absolute correlation between each feature set

min_maxabscors <- corMats

# Impute self-correlations for matrix graphic

selfcors <- data.frame(V1 = c("catch22", "feasts", "tsfeatures", "Kats", "tsfresh", "TSFEL"),
                       V2 = c("catch22", "feasts", "tsfeatures", "Kats", "tsfresh", "TSFEL"),
                       maxcor = c(1, 1, 1, 1, 1, 1))

mean_maxabscors <- bind_rows(mean_maxabscors, selfcors)
min_maxabscors <- bind_rows(min_maxabscors, selfcors)

#------------------ Graphical summary ---------------

# Mean

p <- mean_maxabscors %>%
  ggplot(aes(x = V1, y = V1, fill = maxcor)) +
  geom_tile(aes(width = 0.9, height = 0.9), stat = "identity", alpha = 0.7) +
  geom_text(aes(label = round(maxcor, digits = 2)), colour = "white") +
  labs(x = "Feature Set",
       y = "Feature Set") +
  scale_fill_stepsn(n.breaks = 6, colours = rev(RColorBrewer::brewer.pal(6, "RdYlBu"))) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)

# Min

p1 <- min_maxabscors %>%
  ggplot(aes(x = V1, y = V2, fill = maxcor)) +
  geom_tile(aes(width = 0.9, height = 0.9), stat = "identity", alpha = 0.7) +
  geom_text(aes(label = round(maxcor, digits = 2)), colour = "white") +
  labs(x = "Feature Set",
       y = "Feature Set") +
  scale_fill_stepsn(n.breaks = 6, colours = rev(RColorBrewer::brewer.pal(6, "RdYlBu"))) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p1)
