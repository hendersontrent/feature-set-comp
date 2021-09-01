#--------------------------------------
# This script sets out to produce a
# feature-by-feature correlation
# matrix for certain feature sets
#
# NOTE: setup.R must be run first
#--------------------------------------

#------------------------------------------
# Author: Trent Henderson, 1 September 2021
#------------------------------------------

# Load in feature matrix and filter to tsfresh and TSFEL

load("data/Emp1000FeatMat.Rda")

Emp1000FeatMat2 <- Emp1000FeatMat %>%
  filter(method %in% c("tsfresh", "TSFEL"))

rm(Emp1000FeatMat)

#------------------ Calculations --------------------

source("R/utility_functions.R")

# Check which time series every feature evaluated for and filter if necessary

sets <- get_consistent_datasets_feats(Emp1000FeatMat2)

Emp1000FeatMat2 <- Emp1000FeatMat2 %>%
  filter(id %in% sets)

#-----------------------
# Calculate correlations
#-----------------------

# Normalise and set up data table format

normed <- normalise_feature_frame(Emp1000FeatMat2, names_var = "names", values_var = "values",
                                  method = "z-score") %>%
  mutate(comb_id = paste0(method,"_",names)) %>%
  dplyr::select(c(comb_id, method, values))

normedDT <- data.table(normed)
setkey(normedDT, method) 
normedDT2 <- data.table(normed)
setkey(normedDT2, comb_id)
rm(normed)

#' Function to drive the computations and save outputs in environment
#' 
#' @param dataset1 the data.table containing feature set indexed data
#' @param dataset2 the data.table containing individual feature indexed data
#' @param x the first set of interest
#' @param y the second set of interest
#' @param my_cor the type of correlation to compute
#' @author Trent Henderson
#' 

return_cor_mat2 <- function(dataset1, dataset2, x, y, my_cor = c("pearson", "spearman")){
  
  mat1 <- as.data.frame(make_pairwise_matrix(dataset = dataset1, x = x, y = y))
  
  corMat <- mat1 %>%
    group_by(V1, V2) %>%
    summarise(correlation = return_cor(dataset = dataset2, x = V1, y = V2, cor_type = my_cor)) %>%
    ungroup()
  
  return(corMat)
}

# tsfresh

tsfresh <- return_cor_mat2(normedDT, normedDT2, "tsfresh", "tsfresh", "spearman")

# TSFEL

tsfel <- return_cor_mat2(normedDT, normedDT2, "TSFEL", "TSFEL", "spearman")

#------------------ Plotting ------------------------

#' Function to hierarchically cluster and plot heatmap for a feature set
#' 
#' @param dataset the dataframe to operate on
#' @return an object of class ggplot
#' @author Trent Henderson
#' 

plot_cor_matrix <- function(dataset){
  
  # Widen matrix
  
  tmp <- tsfresh %>%
    pivot_wider(id_cols = "V1", names_from = "V2", values_from = "correlation") %>%
    column_to_rownames(var = "V1")
  
  dat_filtered <- tmp[, which(colMeans(!is.na(tmp)) > 0.1)]
  
  # Filter to just these features
  
  thenames <- colnames(dat_filtered)
  
  tmp2 <- tmp %>%
    rownames_to_column(var = "V1") %>%
    filter(V1 %in% thenames) %>%
    pivot_longer(cols = !V1, names_to = "V2", values_to = "correlation") %>%
    drop_na() %>%
    pivot_wider(id_cols = "V1", names_from = "V2", values_from = "correlation") %>%
    column_to_rownames(var = "V1") %>%
    dplyr::select(all_of(thenames))
  
  # Perform clustering
  
  row.order <- stats::hclust(stats::dist(tmp2))$order # Hierarchical cluster on rows
  col.order <- stats::hclust(stats::dist(t(tmp2)))$order # Hierarchical cluster on columns
  dat_new <- tmp[row.order, col.order] # Re-order matrix by cluster outputs
  cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe
  
  # Draw plot
  
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
          axis.text.x = element_text(angle = 90, size = 3),
          axis.text.y = element_text(size = 3))
  
  return(p)
}

p <- plot_cor_matrix(dataset = tsfresh)
print(p)

p1 <- plot_cor_matrix(dataset = tsfel)
print(p1)



library(gplots)
library(RColorBrewer)
colorscheme <- rev(brewer.pal(8, "RdBu"))

p1 <- gplots::heatmap.2(as.matrix(tmp2), 
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
