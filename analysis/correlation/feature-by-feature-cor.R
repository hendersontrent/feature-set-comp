#--------------------------------------
# This script sets out to produce a
# feature-by-feature correlation
# matrix
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 31 August 2021
#----------------------------------------

# Get list of available pairwise correlation datafiles and bind together

files <- list.files("data/corMats", 
                    full.names = TRUE, pattern = "\\.Rda", all.files = TRUE)

storage <- list()

for(f in files){
  
  message(paste0("Reading in ", f))
  
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

set.seed(1)

x <- sample(unique(corMats$V1), as.integer(0.1*length(unique(corMats$V1))))

# Make a list of self-correlations

selfcors <- data.frame(V1 = unique(corMats$V1)) %>%
  mutate(V2 = V1,
         correlation = 1)

# Convert to matrix

corMats2 <- bind_rows(corMats, selfcors) %>%
  filter(V1 %in% x) %>%
  filter(V2 %in% x) %>%
  drop_na() %>%
  pivot_wider(id_cols = "V1", names_from = "V2", values_from = "correlation") %>%
  column_to_rownames(var = 'V1')

rm(corMats, selfcors) # Clean up environment for memory management

# Perform clustering

row.order <- stats::hclust(stats::dist(corMats2), method = "complete")$order # Hierarchical cluster on rows
col.order <- stats::hclust(stats::dist(t(corMats2)), method = "complete")$order # Hierarchical cluster on columns
dat_new <- corMats2[row.order, col.order] # Re-order matrix by cluster outputs
cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe

#------------------ Plotting ------------------------

p <- cluster_out %>%
  ggplot(aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  labs(x = "Feature",
       y = "Feature",
       fill = TeX("$\\rho$")) +
  scale_fill_distiller(palette = "RdBu", limits = c(-1,1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text.x = element_text(angle = 90, size = 2),
        axis.text.y = element_text(size = 2))

print(p)

library(gplots)
library(RColorBrewer)
colorscheme <- rev(brewer.pal(8, "RdBu"))

p1 <- gplots::heatmap.2(as.matrix(corMats2), 
                  dendrogram = "both",
                  density.info = "none",
                  trace = "none",
                  breaks = seq(-1, 1, length.out = 9),
                  col = colorscheme,
                  labRow = FALSE,
                  labCol = FALSE,
                  revC = TRUE,
                  key = FALSE)

print(p1)

# Save plots

ggsave("output/feature-by-feature.png", p, units = "in", height = 10, width = 10)
ggsave("output/feature-by-feature.svg", p)
