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
#' @param perplexity the perplexity hyperparameter to use if t-SNE algorithm is selected. Defaults to 30
#' @return ggplot2 graphic object containing matrix of plots in two dimensions
#' @author Trent Henderson
#' 

full_low_dim <- function(data, low_dim_method = c("PCA", "tSNE", "UMAP"), perplexity = 30){
  
  # Run calculations
  
  problems <- unique(data$problem)
  storage <- list()
  
  for(i in problems){
    
    if(method == "tSNE"){
      
      tmp <- data %>%
        filter(problem == i)
      
      # Norm the data
      
      normed <- tmp %>%
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
      
      # Fit t-SNE
      
      set.seed(123)
      
      tsneOut <- Rtsne::Rtsne(as.matrix(dat_filtered), perplexity = perplexity, max_iter = 5000, dims = 2,
                              check_duplicates = FALSE)
      
      # Retrieve 2-dimensional embedding and add in unique IDs
      
      id_ref <- dat_filtered %>%
        tibble::rownames_to_column(var = "id") %>%
        dplyr::select(c(id))
      
      fits <- data.frame(.fitted1 = tsneOut$Y[,1],
                         .fitted2 = tsneOut$Y[,2]) %>%
        dplyr::mutate(id = id_ref$id) %>%
        mutate(problem = i)
      
      storage[[i]] <- fits
      
    } else if(method == "UMAP"){
      
      tmp <- data %>%
        filter(problem == i)
      
      # Norm the data
      
      normed <- tmp %>%
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
      
      # Fit UMAP
      
      set.seed(123)
      
      umapOut <- umap(as.matrix(dat_filtered))
      
      # Retrieve 2-dimensional embedding and add in unique IDs
      
      id_ref <- dat_filtered %>%
        tibble::rownames_to_column(var = "id") %>%
        dplyr::select(c(id))
      
      fits <- as.data.frame(umapOut$layout) %>%
        rename(.fitted1 = 1,
               .fitted2 = 2) %>%
        dplyr::mutate(id = id_ref$id) %>%
        mutate(problem = i)
      
      storage[[i]] <- fits
      
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
    
    p <- outs %>%
      ggplot(aes(x = .fitted1, y = .fitted2)) %>%
      geom_point() +
      labs(title = low_dim_method,
           x = "Dimension 1",
           y = "Dimension 2") +
      scale_color_manual(values = available_colours) +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      facet_wrap(~problem, ncol = 5)
    print(p)
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
