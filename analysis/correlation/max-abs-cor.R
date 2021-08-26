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

# Compute min absolute correlation between each feature set

min_abscors <- corMats2 %>%
  mutate(correlation = abs(correlation)) %>%
  group_by(V1, feature_set_source, feature_set_target) %>%
  summarise(min = min(correlation, na.rm = TRUE)) %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  group_by(feature_set_source, feature_set_target) %>%
  summarise(correlation = min(min, na.rm = TRUE)) %>%
  ungroup()

min_abscors2 <- corMats2 %>%
  mutate(correlation = abs(correlation)) %>%
  group_by(V2, feature_set_source, feature_set_target) %>%
  summarise(min = min(correlation, na.rm = TRUE)) %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  group_by(feature_set_source, feature_set_target) %>%
  summarise(correlation = min(min, na.rm = TRUE)) %>%
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
min_abscors <- bind_rows(min_abscors, min_abscors2, selfcors)

#------------------ Graphical summary ---------------

#-----
# Mean
#-----

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
  geom_text(aes(label = round(correlation, digits = 2)), colour = "white", fontface = "bold") +
  labs(x = "Feature Set",
       y = "Feature Set",
       fill = "Mean Max. Abs. Correlation") +
  scale_fill_stepsn(n.breaks = 6, colours = rev(RColorBrewer::brewer.pal(6, "RdYlBu"))) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)

#----
# Min
#----

p1 <- min_abscors %>%
  ggplot(aes(x = feature_set_source, y = feature_set_target, fill = correlation)) +
  geom_tile(aes(width = 0.9, height = 0.9), stat = "identity") +
  geom_text(aes(label = round(correlation, digits = 2)), colour = "white") +
  labs(x = "Feature Set",
       y = "Feature Set",
       fill = "Min. Abs. Correlation") +
  scale_fill_stepsn(n.breaks = 6, colours = rev(RColorBrewer::brewer.pal(6, "RdYlBu"))) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p1)

#------------------
# Mean as a network
#------------------

# Remove self-correlations

graphDat <- mean_maxabscors %>%
  filter(feature_set_source != feature_set_target)

# Set up node and edge data

edges <- graphDat %>%
  rename(from = feature_set_source,
         to = feature_set_target,
         weight = correlation) %>%
  mutate(weight = ifelse(weight >= 0.8, weight*1.75, weight)) # For graphical emphasis

nodes <- graphDat %>%
  dplyr::select(c(feature_set_source)) %>%
  distinct() %>%
  rename(id = feature_set_source)

# Set up graph design

links <- aggregate(edges[,3], edges[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[3] <- "weight"
rownames(links) <- NULL

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

# Draw plot

V(net)$size <- 40
V(net)$frame.color <- "white"
V(net)$color <- "#7570B3"
E(net)$color <- ifelse(E(net)$weight >= 0.8, 'black', 'gray50')
E(net)$width <- E(net)$weight*2.5
mylayout <- layout_with_fr(net)

png("output/network.png", 800, 600)
plot(net, mark.groups = c(2,3,4), mark.col = "#d1ebe3", mark.border = NA,
     layout = mylayout, vertex.label.color = "white",
     edge.curved = .3, edge.arrow.size = .8)
legend("bottomleft", legend = c("rho >= 0.8", "rho < 0.8"))

dev.off()

#-----------
# Save plots
#-----------

ggsave("output/mean-max-abs-cor.png", p)
ggsave("output/mean-max-abs-cor.svg", p)
ggsave("output/mean-max-abs-cor.pdf", p)
ggsave("output/min-abs-cor.png", p1)
