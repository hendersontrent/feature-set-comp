#--------------------------------------
# This script sets out to produce
# data visualisations for relationships
# between features within different
# feature sets
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 13 August 2021
#----------------------------------------

#--------------- Produce graphics ---------------

#' Function to produce a data visualisation of relationship distribution for an input file
#' 
#' @param filepath the path to the dataset of interest
#' @param mytitle the title of the plot
#' @return an object of class ggplot
#' @author Trent Henderson
#' 

plot_similarity_distributions <- function(filepath, mytitle){
  
  tmp <- load(filepath)
  
  tmp <- tmp %>%
    group_by(pearson) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    ggplot(aes(x = pearson, y = counter)) +
    geom_bar(stat = "identity", alpha = 0.9) +
    labs(title = mytitle,
         x = "Pearson Correlation Coefficient",
         y = "Frequency") +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw()
  
  return(p)
}
