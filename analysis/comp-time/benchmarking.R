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
  
  x
  
  #--------- feasts ------------
  
  x
  
  #--------- tsfeatures --------
  
  x
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
