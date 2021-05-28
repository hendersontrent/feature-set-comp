#------------------------------------
# This script sets out to produce a
# low dimension deep-dive of catch22
# using PCA, t-SNE and UMAP
#
# NOTE: setup.R must be run first
#------------------------------------

#-------------------------------------
# Author: Trent Henderson, 28 May 2021
#-------------------------------------

# Read in data

load("data/featureMatrix.Rda")

# Filter to just catch22

caught22 <- featureMatrix %>%
  filter(method == "catch22")

# Drop big dataframe from memory

rm(featureMatrix)

#-------------- Low dimension ---------------

#' Function to calculate and plot low dimension representations for each time-series problem
#' 
#' @param data the dataframe of feature calculations to use
#' @param low_dim_method the low dimension representation algorithm to use
#' @return ggplot2 graphic object containing matrix of plots in two dimensions
#' @author Trent Henderson
#' 

full_low_dim <- function(data, low_dim_method = c("PCA", "tSNE", "UMAP")){
  
  # Run calculations
  
  problems <- unique(data$problem)
  storage <- list()
  
  for(i in problems){
    
    if(method == "tSNE"){
      
      x
      
    } else if(method == "UMAP"){
      
      x
      
    } else{
      tmp1 <- data %>%
        filter(problem == i)
      
      myplot <- theft::plot_low_dimension(data = tmp1, is_normalised = FALSE, id_var = "id", group_var = "target", method = "z-score", plot = TRUE) +
        labs(title = i,
             subtitle = NULL,
             caption = NULL)
      
      storage[[s]] <- myplot
    }
    
  }
  
  if(method != "PCA"){
   
    outs <- rbindlist(storage, use.names = TRUE) 
    
  } else{
    
  }
  
  # Draw plots and store output
  
  CairoPNG(paste0("output/catch22_lowdim_",low_dim_method,".png"), 1200, 1200)
  if(method != "PCA"){
    
    # Define a nice colour palette
    # Palette from https://colorbrewer2.org/#type=qualitative&scheme=Paired&n=12
    
    available_colours <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                           "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6v", "#6a3d9a",
                           "#ffff99", "#b15928")
    
    outs %>%
      ggplot(aes(x = dim1, y = dim2)) %>%
      geom_point() +
      labs(title = low_dim_method,
           x = "Dimension 1",
           y = "Dimension 2") +
      scale_color_manual(values = available_colours) +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      facet_wrap(~problem, ncol = 5)
  } else{
    n <- length(storage)
    ncols <- floor(sqrt(n))
    do.call("grid.arrange", c(storage, ncol = ncols))
  }
  dev.off()
  
}

#-------------- Run function ----------------

full_low_dim(data = caught22, low_dim_method = "PCA")
full_low_dim(data = caught22, low_dim_method = "tSNE")
full_low_dim(data = caught22, low_dim_method = "UMAP")
