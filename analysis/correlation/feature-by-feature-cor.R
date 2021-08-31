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

# Convert to matrix

corMats3 <- corMats2 %>%
  dplyr::select(-c(feature_set_source, feature_set_target)) %>%
  drop_na() %>%
  pivot_wider(id_cols = "V1", names_from = "V2", values_from = "correlation") %>%
  dplyr::select(-c(V1))

# Perform clustering

row.order <- stats::hclust(stats::dist(corMats3), method = "complete")$order # Hierarchical cluster on rows
col.order <- stats::hclust(stats::dist(t(corMats3)), method = "complete")$order # Hierarchical cluster on columns
dat_new <- corMats3[row.order, col.order] # Re-order matrix by cluster outputs
cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe

#------------------ Plotting ------------------------

p <- cluster_out %>%
  ggplot(aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  labs(x = "Feature",
       y = "Feature",
       fill = TeX("$\\rho$")) +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text = element_blank(),
        text = element_text(size = 18))

print(p)

# Save plots

ggsave("output/feature-by-feature.png", p, units = "in", height = 10, width = 10)
ggsave("output/feature-by-feature.svg", p, units = "in", height = 10, width = 10)
