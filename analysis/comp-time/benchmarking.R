#--------------------------------------
# This script sets out to produce
# computation time benchmarking for
# all feature sets on the datasets that
# all successfully compute features on
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 17 August 2021
#----------------------------------------

# Read in data

load("data/empirical1000.Rda")

# Fix Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

# Filter to just good time series

#' Function to return only good time series
#' 
#' @return an object of class vector
#' @author Trent Henderson
#' 

get_good_ts <- function(){
  
  # Load feature matrix
  
  load("data/Emp1000FeatMat.Rda")
  
  # Load hctsa results
  
  source("webscraping/pull_hctsa_results.R")
  hctsa <- pull_hctsa_results() 
  
  # Merge together and remove erroneous duplicates
  
  fullFeatMat <- bind_rows(Emp1000FeatMat, hctsa) %>%
    dplyr::select(c(id, names, method, values)) %>%
    distinct()
  
  # Retain only datasets on which all feature sets successfully computed
  
  source("R/utility_functions.R")
  
  good_datasets <- get_consistent_datasets()
  return(good_datasets)
}

good_datasets <- get_good_ts()

empirical1000_filt <- empirical1000 %>%
  filter(id %in% good_datasets)

#------------------ Benchmarking ----------------

#' Helper function to run computation time tracking
#' 
#' @param data the dataframe to compute features on
#' @param feature_set the feature set to compute
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

compute_time <- function(data, feature_set){
  
  themax <- max(data$timepoint)
  
  m <- summary(microbenchmark(calculate_features(data = data, 
                                                 id_var = "id", 
                                                 time_var = "timepoint", 
                                                 values_var = "values", 
                                                 group_var = NULL,
                                                 feature_set = feature_set),
                              times = 1, unit = "s"))
  
  mdat <- data.frame(m) %>%
    dplyr::select(c(mean, min, max)) %>%
    mutate(ts_length = themax,
           feature_set = feature_set)
  
  return(mdat)
}

#' Function to calculate computation times for each feature set
#' 
#' @param the_set the feature set to compute
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

benchmark_features <- function(the_set = NULL){
  
  # Dummy to burn in instantiation
  
  
  
  # Run actual computations
  
  comptime <- compute_time()
  
  return(comptime)
}


# Run the function for each feature set and bind together for plotting

catch22 <- benchmark_features(the_set = "catch22")
feasts <- benchmark_features(the_set = "feasts")
tsfeatures <- benchmark_features(the_set = "tsfeatures")
kats <- benchmark_features(the_set = "Kats")
tsfresh <- benchmark_features(the_set = "tsfresh")
tsfel <- benchmark_features(the_set = "TSFEL")

all_features <- bind_rows(catch22, feasts, tsfeatures, kats, tsfresh, tsfel)

#------------------ Graphical summary -----------

p <- all_features %>%
  ggplot() +
  geom_errorbar(aes(x = ts_length, ymin = min, ymax = max, colour = feature_set), width = 0.1) +
  geom_line(aes(x = ts_length, y = mean, colour = feature_set)) +
  geom_point(aes(x = ts_length, y = mean, colour = feature_set), size = 2) +
  labs(x = "Time Series Length",
       y = "Computation Time (s)",
       colour = NULL) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 4),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(limits = c(1e-3, 1e3),
                breaks = scales::trans_breaks("log10", function(x) 10^x, n = 7),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(p)

ggsave("output/comp-time.png", p)
