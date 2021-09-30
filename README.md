# feature-set-comp

[![DOI](https://zenodo.org/badge/365082962.svg)](https://zenodo.org/badge/latestdoi/365082962)

Compares various time-series feature sets on computational performance, within-set structure, and between-set relationships.

## Repository Structure

The entire project folder structure and package loads are handled by the `setup.R` script. Please run this before any other scripts and each time you load up the project.

The main chunk of the repository is largely organised around the `analysis/` folder which contains subfolders for each discrete topic of analysis:

* `feature-calcs/` - Computation of time-series features
* `comp-time/` - Benchmarking of feature set evaluation speed
* `correlation/` - Pairwise feature-feature relationships across feature sets
* `redundancy/` - Principal components analysis of within-set feature composition

The `webscraping/` folder contains all the scripts necessary to automatically download and process the Empirical 1000 dataset used in this project.

The `R/` folder contains a collection of functions that were written and reused throughout the project.
