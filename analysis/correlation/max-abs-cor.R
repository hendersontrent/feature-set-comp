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

rm(corMat, storage, f, files)

#------------------ Calculations --------------------

# Get feature set labels

corMats2 <- corMats %>%
  mutate(feature_set_source = gsub("_.*", "\\1", V1),
         feature_set_target = gsub("_.*", "\\1", V2))

rm(corMats)

#------------------------------
# Compute mean maximum absolute 
# correlation between each 
# feature set
#------------------------------

# Part I

mean_maxabscors <- corMats2 %>%
  mutate(correlation = abs(correlation)) %>%
  group_by(V1, feature_set_source, feature_set_target) %>%
  summarise(max = max(correlation, na.rm = TRUE)) %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  group_by(feature_set_source, feature_set_target) %>%
  summarise(correlation = mean(max, na.rm = TRUE)) %>%
  ungroup()

# Part II (asymmetric matrix)

mean_maxabscors2 <- corMats2 %>%
  mutate(correlation = abs(correlation)) %>%
  group_by(V2, feature_set_source, feature_set_target) %>%
  summarise(max = max(correlation, na.rm = TRUE)) %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  group_by(feature_set_source, feature_set_target) %>%
  summarise(correlation = mean(max, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(V1 = 1,
         V2 = 2) %>%
  rename(feature_set_source = V2,
         feature_set_target = V1)

# Impute self-correlations for matrix graphic

selfcors <- data.frame(feature_set_source = c("catch22", "feasts", "tsfeatures", "Kats", "tsfresh", "TSFEL", "hctsa"),
                       feature_set_target = c("catch22", "feasts", "tsfeatures", "Kats", "tsfresh", "TSFEL", "hctsa"),
                       correlation = c(1, 1, 1, 1, 1, 1, 1))

mean_maxabscors <- bind_rows(mean_maxabscors, mean_maxabscors2, selfcors)

#------------------ Graphical summary ---------------

#---------
# Mean max
#---------

# Convert to factor prior to plotting for easier interpretation

p <- mean_maxabscors %>%
  mutate(feature_set_source = factor(feature_set_source, 
                                     levels = c("catch22", "feasts", "Kats", "tsfeatures",
                                                "hctsa", "TSFEL", "tsfresh"))) %>% 
  mutate(feature_set_target = factor(feature_set_target, 
                                     levels = c("catch22", "feasts", "Kats", "tsfeatures",
                                                "hctsa", "TSFEL", "tsfresh"))) %>% 
  ggplot(aes(x = feature_set_source, y = feature_set_target, fill = correlation)) +
  geom_tile(aes(width = 0.9, height = 0.9), stat = "identity") +
  geom_text(aes(label = round(correlation, digits = 2)), colour = "black", fontface = "bold",
            size = 6) +
  labs(x = "Test",
       y = "Benchmark",
       fill = TeX("$|\\rho_{max}|$")) +
  scale_fill_distiller(palette = "GnBu", direction = 1,
                       limits = c(0.6,1), oob = squish) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        legend.text = element_text(size = 12))

print(p)

#-----------
# Save plots
#-----------

ggsave("output/mean-max-abs-cor.png", p, units = "in", height = 10, width = 10)
ggsave("output/mean-max-abs-cor.svg", p, units = "in", height = 10, width = 10)
ggsave("output/mean-max-abs-cor.pdf", p)
