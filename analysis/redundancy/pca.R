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
  
  the_sets <- unique(data$method)
  storage <- list()
  
  # Iterate through each set
  
  for(i in the_sets){
    
    tryCatch({
    
    # Filter to set and normalise
    
    tmp <- data %>%
      filter(method == i) %>%
      dplyr::select(c(id, names, values)) %>%
      distinct() %>%
      drop_na() %>%
      group_by(names) %>%
      mutate(values = normalise_feature_vector(values, method = "RobustSigmoid")) %>%
      ungroup() %>%
      drop_na()
    
    # Widen the matrix
    
    dat <- tmp %>%
      pivot_wider(id_cols = id, names_from = names, values_from = values) %>%
      tibble::column_to_rownames(var = "id")
    
    # Remove any columns with >50% NAs to prevent masses of rows getting dropped due to poor features
    
    dat_filtered <- dat[, which(colMeans(!is.na(dat)) > 0.5)]
    
    # Drop any remaining rows with NAs
    
    dat_filtered <- dat_filtered %>%
      drop_na()
    
    # Compute PCA
    
    fits <- dat_filtered %>%
      prcomp(scale = FALSE)
    
    eigenvalues <- fits %>%
      tidy(matrix = "eigenvalues") %>%
      dplyr::select(c(PC, percent)) %>%
      mutate(feature_set = i)
    
    # Store output
    
    storage[[i]] <- eigenvalues
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  pc_results <- rbindlist(storage, use.names = TRUE)
  return(pc_results)
}

# Run function and compute cumulative sum for later

pca_results <- do_pca_summary(Emp1000FeatMat) %>%
  group_by(feature_set) %>%
  arrange(PC) %>%
  mutate(cs = cumsum(percent)) %>%
  ungroup()

#-------------- Produce summary graphic ----------------

# Regular

p <- pca_results %>%
  ggplot(aes(x = PC, y = (percent*100), colour = feature_set)) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Principal Component",
       y = "Variance Explained (%)",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)

# Cumulative sum

p1 <- pca_results %>%
  ggplot(aes(x = PC, y = (cs*100), colour = feature_set)) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Principal Component",
       y = "Cumulative Variance Explained (%)",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p1)

# Scaled by number of features

p2 <- pca_results %>%
  group_by(feature_set) %>%
  mutate(PC = PC/sum(PC)) %>%
  ungroup() %>%
  ggplot(aes(x = PC, y = (percent*100), colour = feature_set)) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Principal Component",
       y = "Variance Explained (%)",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p2)

p3 <- pca_results %>%
  group_by(feature_set) %>%
  mutate(PC = PC/sum(PC)) %>%
  ungroup() %>%
  ggplot(aes(x = PC, y = (cs*100), colour = feature_set)) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Principal Component / Total Number of Components",
       y = "Cumulative Variance Explained (%)",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p3)

# Save plots

ggsave("output/pca.png", p)
ggsave("output/pca-cumsum.png", p1)
ggsave("output/pca-scaled.png", p2)
ggsave("output/pca-cumsum-scaled.png", p3)
