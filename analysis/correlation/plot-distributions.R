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

plot_similarity_distributions <- function(filepath){
  
  load(filepath)
  
  p <- corMat %>%
    drop_na() %>%
    ggplot(aes(x = pearson)) +
    geom_histogram(alpha = 0.8, fill = "#D95F02") +
    labs(title = paste0("Distribution of feature correlations between ", 
                        gsub("_.*","",corMat[1,1]), " and ", gsub("_.*","",corMat[1,2])),
         subtitle = "Each datapoint is a feature x feature Pearson correlation",
         x = "Pearson Correlation Coefficient",
         y = "Frequency") +
    theme_bw()
  
  return(p)
}

#--------
# catch22
#--------

p <- plot_similarity_distributions("/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/1.Rda")
p1 <- plot_similarity_distributions("/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/2.Rda")
p2 <- plot_similarity_distributions("/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/3.Rda")
p3 <- plot_similarity_distributions("/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/4.Rda")
p4 <- plot_similarity_distributions("/Users/trenthenderson/Dropbox/Manuscripts/feature-set-comp/5.Rda")

ggsave("output/catch22_feasts.png", p)
ggsave("output/catch22_tsfeatures.png", p1)
ggsave("output/catch22_Kats.png", p2)
ggsave("output/catch22_tsfeatures.png", p3)
ggsave("output/catch22_TSFEL.png", p4)

#--------
# feasts
#--------



#-----------
# tsfeatures
#-----------



#-----
# Kats
#-----



#--------
# tsfresh
#--------



#------
# TSFEL
#------



#------
# hctsa
#------


