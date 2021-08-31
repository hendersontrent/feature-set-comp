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

#---------------
# Compute times
# for R packages
#---------------

#' Function to track computation time for Rcatch22, feasts, and tsfeatures
#' 
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

# NOTE: ONLY RUN THIS IF "output/comptime/R.csv" DOES NOT EXIST!

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
  rm(tmp, x, m, results)
  
  #--------- feasts ------------
  
  message("Doing feasts...")
  
  for(f in files){
    
    tmp <- readr::read_csv(f)
    x1 <- tsibble::as_tsibble(tmp, index = X1)
    
    m <- summary(microbenchmark(x1 %>% fabletools::features(values, fabletools::feature_set(pkgs = "feasts")), times = 1, unit = "s"))
    
    results <- data.frame(m) %>%
      dplyr::select(c(mean)) %>%
      mutate(ts_length = nrow(x1),
             feature_set = "feasts")
    
    storage_fe[[f]] <- results
  }
  
  results_fe <- data.table::rbindlist(storage_fe, use.names = TRUE)
  rm(tmp, x, x1, m, results)
  
  #--------- tsfeatures --------
  
  message("Doing tsfeatures...")
  
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
  
  outs <- bind_rows(results_c22, results_fe, results_ts)
}

r_pkg_results <- track_comptime()
write.csv(r_pkg_results, "output/comptime/R.csv")

#-------------------
# Read in results
# for non-R packages
#-------------------

# Load files and remove anomalous entries

r_pkg_results <- readr::read_csv("output/comptime/R.csv") # Only run this if it has been created
kats_results <- readr::read_csv("output/comptime/kats.csv") %>% filter(mean < 8)
tsfresh_results <- readr::read_csv("output/comptime/tsfresh.csv")
tsfel_results <- readr::read_csv("output/comptime/tsfel.csv") %>% filter(mean < 0.12)

# Add columns to hctsa vector of times

thelengths <- c(100, 100, 100, 100, 100, 250, 250, 250, 250, 250,
                500, 500, 500, 500, 500, 750, 750, 750, 750, 750,
                1000, 1000, 1000, 1000, 1000)

hctsa_results <- readr::read_csv("output/comptime/outputTimes.csv", col_names = FALSE) %>% 
  rename(mean = X1) %>%
  mutate(feature_set = "hctsa",
         ts_length = thelengths)

#-------------------
# Bind all together
# for plotting and
# compute min, mean,
# max for each
# length
#-------------------

all_comptimes <- bind_rows(r_pkg_results, kats_results, tsfresh_results, tsfel_results, hctsa_results) %>%
  group_by(feature_set, ts_length) %>%
  summarise(mean = mean(mean)) %>%
  ungroup()

#------------------ Graphical summary ---------------

p <- all_comptimes %>%
  ggplot() +
  geom_line(aes(x = ts_length, y = mean, colour = feature_set)) +
  geom_point(aes(x = ts_length, y = mean, colour = feature_set), size = 2) +
  labs(x = "Time Series Length",
       y = "Computation Time (s)",
       colour = NULL) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(breaks = c(1e2, 1e3),
                labels = trans_format("log10", label_math())) +
  scale_y_log10(limits = c(1e-3, 1e2),
                breaks = scales::trans_breaks("log10", function(x) 10^x, n = 6),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(p)

ggsave("output/comp-time.png", p)
ggsave("output/comp-time.svg", p)
ggsave("output/comp-time.pdf", p)
