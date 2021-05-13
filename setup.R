#------------------------------------
# This script sets out to load all
# the packages and folders necessary
# for the project
#------------------------------------

#------------------------------------
# Author: Trent Henderson, 7 May 2021
#------------------------------------

library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(scales)
library(foreign)
library(Rcatch22) # devtools::install_github("hendersontrent/Rcatch22")
library(theft) # devtools::install_github("hendersontrent/theft")
library(Cairo)
library(reticulate)
library(gridExtra)

# Create important folders if none exist

if(!dir.exists('webscraping')) dir.create('webscraping')
if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('data')) dir.create('data')
