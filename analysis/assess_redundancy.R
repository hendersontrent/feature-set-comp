#------------------------------------
# This script sets out to produce a
# PCA analysis of each method to 
# assess feature redundancy
#
# NOTE: setup.R must be run first
#------------------------------------

#-------------------------------------
# Author: Trent Henderson, 28 May 2021
#-------------------------------------

# Read in data

load("data/featureMatrix.Rda")

#-------------- PCA ---------------

#' Function to calculate PCA for each method and plot as a comparison
#' 
#' @param data the dataframe of feature calculations to use
#' @return ggplot2 graphic object containing matrix of plots of variance explained against feature numbers
#' @author Trent Henderson
#' 

assess_redundancy <- function(data){
  
  tmp <- data %>%
    filter(problem == "MoteStrain")
  
  #----------- Calculate PCA for each method -----------
  
  methoder <- unique(data$method)
  storage <- list()
  
  for(m in methoder){
    
    tmp1 <- tmp %>%
      filter(method == m)
    
    # Norm the data
    
    normed <- tmp1 %>%
      dplyr::select(c(id, names, values)) %>%
      dplyr::group_by(names) %>%
      dplyr::mutate(values = normalise_feature_vector(values, method = "z-score")) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()
    
    # Produce matrix
    
    dat <- normed %>%
      tidyr::pivot_wider(id_cols = id, names_from = names, values_from = values) %>%
      tibble::column_to_rownames(var = "id")
    
    # Remove any columns with all NAs to avoid whole dataframe being dropped
    
    dat_filtered <- dat[colSums(!is.na(dat)) > 0]
    
    # Drop any remaining rows with NAs
    
    dat_filtered <- dat_filtered %>%
      tidyr::drop_na()
    
    # PCA calculation
    
    set.seed(123)
    
    pca_fit <- dat_filtered %>%
      stats::prcomp(scale = FALSE)
    
    # Retrieve eigenvalues and tidy up variance explained
    
    eigens <- pca_fit %>%
      broom::tidy(matrix = "eigenvalues") %>%
      dplyr::select(c(PC, percent)) %>%
      dplyr::mutate(method = m) %>%
      dplyr::mutate(percent = percent*100)
    
    # Store results
    
    storage[[m]] <- eigens
  }
  
  outs <- rbindlist(storage, use.names = TRUE)
  
  # Calculate a cumulative sum
  
  outs2 <- outs %>%
    group_by(method) %>%
    arrange(PC) %>%
    mutate(cumsum = cumsum(percent)) %>%
    ungroup()
  
  #---------- Draw graphic -----------
  
  # Define a nice colour palette
  # Palette from https://colorbrewer2.org/#type=qualitative&scheme=Paired&n=12
  
  available_colours <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                         "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6v", "#6a3d9a",
                         "#ffff99", "#b15928")
  
  CairoPNG("output/feature_redundancy.png", 800, 800)
  p <- outs2 %>%
    ggplot(aes(x = PC, y = cumsum, group = method, colour = method)) +
    geom_point() +
    geom_line() +
    labs(title = "Variance explained by principal components for each feature set",
         x = "Principal Components",
         y = "Cumulative Variance Explained",
         colour = "Feature Set") +
    scale_y_continuous(labels = function(x)paste0(x,"%")) +
    scale_color_manual(values = available_colours) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom")
  print(p)
  dev.off()
}

# Run function

assess_redundancy(data = featureMatrix)
