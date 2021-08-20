#--------------------------------------
# This script sets out to produce
# computation time benchmarking analysis
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 17 August 2021
#----------------------------------------

#------------------ Benchmark calculations ----------

#-------------------
# Compute times
# for R packages
#-------------------

#' Function to track computation time for Rcatch22, feasts, and tsfeatures
#' 
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

track_comptime <- function(){
  
  #--------- Rcatch22 ----------
  
  files <- list.files("data/sims", full.names = TRUE, pattern = "\\csv", all.files = TRUE)
  storage_c22 <- list()
  storage_fe <- list()
  storage_ts <- list()
  
  for(f in files){
    
    tmp <- readr::read_csv(f)
    x <- tmp$values
    
    m <- summary(microbenchmark(Rcatch22::catch22_all(x), times = 1, unit = "s"))
    
    results <- data.frame(m) %>%
      dplyr::select(c(mean)) %>%
      mutate(ts_length = length(x),
             feature_set = "catch22")
    
    storage_c22[[f]] <- results
  }
  
  results_c22 <- data.table::rbindlist(storage_c22, use.names = TRUE)
  
  #--------- feasts ------------
  
  #x
  
  #--------- tsfeatures --------
  
  for(f in files){
    
    tmp <- readr::read_csv(f)
    x <- tmp$values
    
    m <- summary(microbenchmark(tsfeatures::tsfeatures(x, 
                                                       features = c("frequency", "stl_features", "entropy", "acf_features",
                                                                    "compengine", "arch_stat", "crossing_points", "flat_spots",
                                                                    "heterogeneity", "holt_parameters", "hurst", 
                                                                    "lumpiness", "max_kl_shift", "max_level_shift", "max_var_shift", 
                                                                    "nonlinearity", "pacf_features", "stability", "unitroot_kpss",
                                                                    "unitroot_pp", "embed2_incircle", "firstzero_ac",
                                                                    "histogram_mode", "localsimple_taures", "sampenc",
                                                                    "spreadrandomlocal_meantaul")), 
                                times = 1, unit = "s"))
    
    results <- data.frame(m) %>%
      dplyr::select(c(mean)) %>%
      mutate(ts_length = length(x),
             feature_set = "tsfeatures")
    
    storage_ts[[f]] <- results
  }
  results_ts <- data.table::rbindlist(storage_ts, use.names = TRUE)
  
  #--------- Binding -----------
  
  outs <- bind_rows(results_c22, results_ts)
}

r_pkg_results <- track_comptime()

#-------------------
# Read in results
# for non-R packages
#-------------------

kats_results <- readr::read_csv("output/comptime/kats.csv")
tsfresh_results <- readr::read_csv("output/comptime/tsfresh.csv")
tsfel_results <- readr::read_csv("output/comptime/tsfel.csv")

#-------------------
# Bind all together
# for plotting
#-------------------

all_comptimes <- bind_rows(r_pkg_results, kats_results, tsfresh_results, tsfel_results)

#------------------ Graphical summary ---------------

p <- all_comptimes %>%
  ggplot() +
  geom_errorbar(aes(x = ts_length, ymin = min, ymax = max, colour = feature_set), width = 0.1) +
  geom_line(aes(x = ts_length, y = mean, colour = feature_set)) +
  geom_point(aes(x = ts_length, y = mean, colour = feature_set), size = 2) +
  labs(x = "Time Series Length",
       y = "Computation Time (s)",
       colour = NULL) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 3),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(limits = c(1e-3, 1e3),
                breaks = scales::trans_breaks("log10", function(x) 10^x, n = 7),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(p)

ggsave("output/comp-time.png", p)
ggsave("output/comp-time.svg", p)
