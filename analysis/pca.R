#------------------------------------
# This script sets out to produce a
# PCA analysis and plot for each
# feature set and problem
#
# NOTE: setup.R must be run first
#------------------------------------

#-------------------------------------
# Author: Trent Henderson, 12 May 2021
#-------------------------------------

# Read in data

load("data/featureMatrix.Rda")

#-------------- PCA ---------------

#' Function to calculate PCA for each feature set and time-series problem
#' 
#' @param data the dataframe of feature calculations to use
#' @return ggplot2 graphic object containing matrix of plots of PCA outputs in two dimensions
#' @author Trent Henderson
#' 

do_pca <- function(data, problem_name = NULL){
  
  tmp <- data %>%
    filter(problem == problem_name)
  
  sets <- unique(tmp$feature_set)
  storage <- list()
  
  # Produce plots
  
  for(s in sets){
    
    tmp1 <- tmp %>%
      filer(feature_set == s)
    
    myplot <- theft::plot_low_dimension(data = tmp1, is_normalised = FALSE, id_var = "id", group_var = "target", method = "z-score", plot = TRUE) +
      labs(title = NULL,
           subtitle = NULL,
           caption = NULL) +
      labs(title = problem_name,
           subtitle = s) # Should probably find a way to produce a single title for all plots
    
    storage[[s]] <- myplot
  }
  
  # Merge into single graphic
  
  library(gridExtra)
  
  n <- length(storage)
  ncols <- floor(sqrt(n))
  p <- do.call("grid.arrange", c(storage, ncol = ncols))
  return(p)
}

# Produce plots for each problem

p <- do_pca(data = featureMatrix, problem_name = "Yoga")
p1 <- do_pca(data = featureMatrix, problem_name = "WormsTwoClass")
p2 <- do_pca(data = featureMatrix, problem_name = "Wine")
p3 <- do_pca(data = featureMatrix, problem_name = "Wafer")
p4 <- do_pca(data = featureMatrix, problem_name = "TwoLeadECG")
p5 <- do_pca(data = featureMatrix, problem_name = "ToeSegmentation2")
p6 <- do_pca(data = featureMatrix, problem_name = "ShapeletSim")
p7 <- do_pca(data = featureMatrix, problem_name = "SemgHandGenderCh2")
p8 <- do_pca(data = featureMatrix, problem_name = "ProximalPhalanxOutlineCorrect")
p9 <- do_pca(data = featureMatrix, problem_name = "")
p10 <- do_pca(data = featureMatrix, problem_name = "")
p11 <- do_pca(data = featureMatrix, problem_name = "")

#-------------- Final PDF ---------

CairoPDF("output/pca.pdf", 11, 8)
print(p)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p9)
dev.off()
