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

# Load hctsa results

source("R/process_hctsa_csv.R")
hctsa <- process_hctsa_csv() 

# Merge together

fullFeatMat <- bind_rows(Emp1000FeatMat, hctsa)

# Clean up environment as files are big

rm(Emp1000FeatMat, hctsa)

# Get list of time series that have 0 NAs across all features after removing features with >10% NAs

source("R/utility_functions.R")

good_ids <- remove_problematic_datasets(fullFeatMat)

#-------------- Preliminary calculations----------------

#---------------------------
# Total number of features 
# by feature set and dataset
#---------------------------

num_feats <- fullFeatMat2 %>%
  dplyr::select(c(id, names, method)) %>%
  distinct() %>%
  group_by(id, names, method) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  dplyr::select(c(names, method)) %>%
  distinct()

#-------------- Do PCA for each feature set ------------

#' Function to produce PCA on each Dataset x Feature matrix and bind results
#' 
#' @param dataset the Dataset x Feature matrix dataframe
#' @return a dataframe containing PC results
#' @author Trent Henderson
#' 

do_pca_summary <- function(dataset){
  
  the_sets <- unique(dataset$method)
  storage <- list()
  
  # Iterate through each set
  
  for(i in the_sets){
    
    tryCatch({
    
    # Filter to set and normalise
    
    tmp <- dataset %>%
      filter(method == i) %>%
      dplyr::select(c(id, names, values)) %>%
      distinct() %>%
      group_by(names) %>%
      mutate(values = normalise_feature_vector(values, method = "z-score")) %>%
      ungroup()
    
    # Widen the matrix
    
    dat <- tmp %>%
      pivot_wider(id_cols = id, names_from = names, values_from = values) %>%
      tibble::column_to_rownames(var = "id")
    
    # Filter features with >10% NAs
    
    dat_filtered <- dat[, which(colMeans(!is.na(dat)) > 0.90)]
    
    message(paste0(i,": Removed ",ncol(dat)-ncol(dat_filtered)," features (",round(((ncol(dat)-ncol(dat_filtered))/ncol(dat))*100, digits = 2),"%)"))
    
    # Remove IDs that aren't good across all features
    
    dat_filtered <- subset(dat_filtered, rownames(dat_filtered) %in% good_ids)
    
    message(paste0(i,": Removed ",nrow(dat)-nrow(dat_filtered)," datasets (",round(((nrow(dat)-nrow(dat_filtered))/nrow(dat))*100, digits = 2),"%)"))
    
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

pca_results <- do_pca_summary(fullFeatMat) %>%
  group_by(feature_set) %>%
  arrange(PC) %>%
  mutate(cs = cumsum(percent)) %>%
  ungroup()

#-------------- Produce summary graphic ----------------

# Bar plot to 90% variance

p <- pca_results %>%
  group_by(feature_set) %>%
  mutate(PC = PC/max(PC)) %>%
  filter(cs > 0.90) %>%
  group_by(feature_set) %>%
  mutate(flag = ifelse(cs == min(cs), "Min", "Not")) %>%
  ungroup() %>%
  filter(flag == "Min") %>%
  ggplot(aes(x = reorder(feature_set, -PC), y = (PC*100), fill = feature_set)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(subtitle = "B",
       x = "Feature set",
       y = "% of principal components for 90% variance",
       fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 90),
        plot.subtitle = element_text(face = "bold"))

print(p)

# Cumulative

p1 <- pca_results %>%
  group_by(feature_set) %>%
  mutate(PC = PC/max(PC)) %>%
  ungroup() %>%
  ggplot(aes(x = (PC*100), y = (cs*100), colour = feature_set)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 90, lty = "dashed") +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(subtitle = "A",
       x = "% of principal components",
       y = "Cumulative variance explained (%)",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        plot.subtitle = element_text(face = "bold"))

print(p1)

# Save plots

p2 <- ggpubr::ggarrange(p1, p, nrow = 2, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("output/pca-merged.pdf", p2, units = "in", height = 13, width = 10)
