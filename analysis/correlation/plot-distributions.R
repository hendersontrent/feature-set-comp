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
#' @return an object of class ggplot
#' @author Trent Henderson
#' 

plot_similarity_distributions <- function(filepath, save = TRUE){
  
  load(filepath)
  
  p <- corMat %>%
    drop_na() %>%
    ggplot(aes(x = pearson)) +
    geom_histogram(alpha = 0.8, fill = "#D95F02", binwidth = 0.05) +
    labs(title = paste0("Distribution of feature correlations between ", 
                        gsub("_.*","",corMat[1,1]), " and ", gsub("_.*","",corMat[1,2])),
         subtitle = "Each datapoint is a feature x feature Pearson correlation",
         x = "Pearson Correlation Coefficient",
         y = "Frequency") +
    theme_bw()
  
  if(save){
    savepath <- paste0("output/",gsub("_.*","",corMat[1,1]), "_", gsub("_.*","",corMat[1,2]),".png")
    ggsave(savepath, p)
  } else{
    return(p)
  }
}

# Get list of available pairwise correlation datafiles and iterate through to plot each one and save

files <- list.files("data/corMat", 
                    full.names = TRUE, pattern = "\\.Rda", all.files = TRUE)

for(f in files){
  plot_similarity_distributions(f, save = TRUE)
}
