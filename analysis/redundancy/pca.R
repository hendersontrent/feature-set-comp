#------------------------------------
# This script sets out to calculate
# dimension reduction properties on
# the extracted feature matrices for
# the Empirical 1000
#
# NOTE: setup.R must be run first
#------------------------------------

#---------------------------------------
# Author: Trent Henderson, 9 August 2021
#---------------------------------------

# Load feature matrix

load("data/Emp1000FeatMat.Rda")

#-------------- Do PCA for each feature set ------------

#' Function to produce PCA on each Dataset x Feature matrix and bind results
#' 
#' @param data the Dataset x Feature matrix dataframe
#' @return a dataframe containing PC results
#' @author Trent Henderson
#' 

do_pca_summary <- function(data){
  
  the_sets <- unique(data$feature_set)
  storage <- list()
  
  # Iterate through each set
  
  for(i in the_sets){
    
    # Filter to set
    
    tmp <- data %>%
      filter(feature_set == i)
    
    # Normalise
    
    tmp <- tmp %>%
      dplyr::select(c(id, names, values)) %>%
      tidyr::drop_na() %>%
      dplyr::group_by(names) %>%
      dplyr::mutate(values = normalise_feature_vector(values, method = "RobustSigmoid")) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()
    
    # Widen the matrix
    
    dat <- normed %>%
      tidyr::pivot_wider(id_cols = id, names_from = names, values_from = values) %>%
      tibble::column_to_rownames(var = "id")
    
    # Remove any columns with >50% NAs to prevent masses of rows getting dropped due to poor features
    
    dat_filtered <- dat[, which(colMeans(!is.na(dat)) > 0.5)]
    
    # Drop any remaining rows with NAs
    
    dat_filtered <- dat_filtered %>%
      tidyr::drop_na()
    
    # Compute PCA
    
    fits <- dat_filtered %>%
      stats::prcomp(scale = FALSE)
    
    eigens <- fits %>%
      broom::tidy(matrix = "eigenvalues") %>%
      dplyr::select(c(PC, percent))
    
    # Reshape from wide to long
    
    reshaped <- myresults %>%
      pivot_longer(cols = 1:1, names_to = "pc")
    
    # Store output
    
    storage[[i]] <- reshaped
  }
  
  pc_results <- rbindlist(storage, use.names = TRUE)
  return(pc_results)
}

# Run function

pca_results <- do_pca_summary(Emp1000FeatMat)

#-------------- Produce summary graphic ----------------

pca_results %>%
  ggplot(aes(x = pc, y = var_expl, colour = feature_set)) +
  geom_line(size = 0.9) +
  geom_point(size = 2.5) +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Principal Component",
       y = "Variance Explained (%)") +
  theme_bw() +
  theme(legend.position = "bottom")
