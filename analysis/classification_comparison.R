#-------------------------------------
# This script sets out to produce
#analysis of classification performance
# by problem and feature set
#
# NOTE: setup.R and fit_classifiers.R 
# must be run first
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 31 May 2021
#-------------------------------------

# Read in classification results

load("data/classifierOutputs.Rda")

# Define a nice colour palette
# Palette from https://www.schemecolor.com/land-of-pastels.php

available_colours <- c("#E494D3", "#87DCC0", "#88BBE4", "#998AD3", "#CDF1AF")

#-------------- Plot 1: All accuracies ---------------

CairoPNG("output/feature-set-accuracy-violin.png", 800, 600)
p <- classifierOutputs %>%
  mutate(accuracy = as.numeric(accuracy)) %>%
  mutate(method = as.factor(method)) %>%
  ggplot(aes(x = method, y = accuracy, colour = method)) +
  geom_violin() +
  geom_jitter() +
  labs(title = "Feature set accuracy for all problems",
       x = "Feature Set",
       y = "Classification Accuracy (%)") +
  scale_y_continuous(labels = function(x)paste0(x,"%")) +
  scale_colour_manual(values = available_colours) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
print(p)
dev.off()

#-------------- Plot 2: Accuracy by problem ----------

CairoPNG("output/feature-set-accuracy-facet.png", 800, 600)
p1 <- classifierOutputs %>%
  mutate(accuracy = as.numeric(accuracy)) %>%
  mutate(method = as.factor(method)) %>%
  ggplot(aes(x = method, y = accuracy, colour = method)) +
  geom_point(size = 2.5) +
  labs(title = "Feature set accuracy by problem",
       x = "Feature Set",
       y = "Classification Accuracy (%)") +
  scale_y_continuous(labels = function(x)paste0(x,"%")) +
  scale_colour_manual(values = available_colours) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  facet_wrap(~problem, ncol = 4)
print(p1)
dev.off()
