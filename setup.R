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
library(tibble)
library(magrittr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(foreign)
library(theft)
library(Cairo)
library(reticulate)
library(broom)

# Create important folders if none exist

if(!dir.exists('webscraping')) dir.create('webscraping')
if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('analysis/feature-calcs')) dir.create('analysis/feature-calcs')
if(!dir.exists('analysis/classification')) dir.create('analysis/classification')
if(!dir.exists('analysis/redundancy')) dir.create('analysis/redundancy')
if(!dir.exists('analysis/correlation')) dir.create('analysis/correlation')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('data')) dir.create('data')
if(!dir.exists('data/corMats')) dir.create('data/corMats')
if(!dir.exists('R')) dir.create('R')
