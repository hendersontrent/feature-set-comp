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
library(latex2exp)
library(foreign)
library(Rcatch22)
library(theft)
library(Cairo)
library(reticulate)
library(broom)
library(microbenchmark)
library(R.matlab)
library(ggpubr)

# Create important folders if none exist

if(!dir.exists('webscraping')) dir.create('webscraping')
if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('analysis/feature-calcs')) dir.create('analysis/feature-calcs')
if(!dir.exists('analysis/redundancy')) dir.create('analysis/redundancy')
if(!dir.exists('analysis/correlation')) dir.create('analysis/correlation')
if(!dir.exists('analysis/comp-time')) dir.create('analysis/comp-time')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('output/comptime')) dir.create('output/comptime')
if(!dir.exists('data')) dir.create('data')
if(!dir.exists('data/corMats')) dir.create('data/corMats')
if(!dir.exists('data/sims')) dir.create('data/sims')
if(!dir.exists('data/sims/kats')) dir.create('data/sims/kats')
if(!dir.exists('data/sims/hctsa')) dir.create('data/sims/hctsa')
if(!dir.exists('data/sims/sinusoid')) dir.create('data/sims/sinusoid')
if(!dir.exists('data/sims/sinusoid/kats')) dir.create('data/sims/sinusoid/kats')
if(!dir.exists('data/sims/sinusoid/hctsa')) dir.create('data/sims/sinusoid/hctsa')
if(!dir.exists('R')) dir.create('R')
