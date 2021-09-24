#--------------------------------------
# This script sets out to produce
# computation time benchmarking analysis
# for noisy sinusoid versus Gaussian noise
#
# NOTE: setup.R must be run first
#--------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 24 September 2021
#-------------------------------------------

#--------------- Benchmark calculations ------------

#---------------
# Compute times
# for R packages
#---------------

#' Function to track computation time for Rcatch22, feasts, and tsfeatures
#' 
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

# NOTE: ONLY RUN THIS IF "output/comptime/sinusoid/R.csv" DOES NOT EXIST!

track_comptime_sinusoid <- function(){
  
  #--------- Rcatch22 ----------
  
  files <- list.files("data/sims/sinusoid", full.names = TRUE, pattern = "\\csv", all.files = TRUE)
  storage_c22 <- list()
  storage_fe <- list()
  storage_ts <- list()
  
  for(f in files){
    
    tmp <- readr::read_csv(f)
    x <- tmp$value
    
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
    
    m <- summary(microbenchmark(x1 %>% fabletools::features(value, fabletools::feature_set(pkgs = "feasts")), times = 1, unit = "s"))
    
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
    x <- tmp$value
    
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

r_pkg_results <- track_comptime_sinusoid()
write.csv(r_pkg_results, "output/comptime/sinusoid/R.csv")

# Load in existing computation times

#' Function to load in Gaussian and noisy sinusoid comp time data
#' 
#' @param R_path string to the csv containing R package results
#' @param tsfresh_path string to the csv containing tsfresh package results
#' @param TSFEL_path string to the csv containing TSFEL package results
#' @param Kats_path string to the csv containing Kats package results
#' @param hctsa_path string to the csv containing hctsa package results
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

grab_comp_times <- function(R_path = NULL, tsfresh_path = NULL, TSFEL_path = NULL, Kats_path = NULL, 
                            hctsa_path = NULL){
  
  # Load files
  
  r_pkg_results <- readr::read_csv(R_path) # Only run this if it has been created
  kats_results <- readr::read_csv(Kats_path)
  tsfresh_results <- readr::read_csv(tsfresh_path)
  tsfel_results <- readr::read_csv(TSFEL_path)
  
  # Add vector of times to hctsa results
  
  thelengths <- c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 
                  250, 250, 250, 250, 250,250, 250, 250, 250, 250,
                  500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 
                  750, 750, 750, 750, 750, 750, 750, 750, 750, 750,
                  1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000)
  
  hctsa_results <- readr::read_csv(hctsa_path, col_names = FALSE) %>% 
    rename(mean = X1) %>%
    mutate(feature_set = "hctsa",
           ts_length = thelengths)
  
  # Bind all together
  
  all_comptimes <- bind_rows(r_pkg_results, kats_results, tsfresh_results, tsfel_results, hctsa_results) %>%
    mutate(feature_set_feats = case_when(
      feature_set == "catch22"    ~ "catch22 (22)",
      feature_set == "feasts"     ~ "feasts (43)",
      feature_set == "hctsa"      ~ "hctsa (7300)",
      feature_set == "Kats"       ~ "Kats (40)",
      feature_set == "tsfeatures" ~ "tsfeatures (62)",
      feature_set == "TSFEL"      ~ "TSFEL (285-390)",
      feature_set == "tsfresh"    ~ "tsfresh (1558)")) %>%
    dplyr::select(-c(X1))
  
  return(all_comptimes)
}

# Run the function

gaussian <- grab_comp_times(R_path = "output/comptime/R.csv",
                            tsfresh_path = "output/comptime/tsfresh.csv",
                            TSFEL_path = "output/comptime/tsfel.csv",
                            Kats_path = "output/comptime/kats.csv",
                            hctsa_path = "output/comptime/outputTimes.csv") %>%
  mutate(unique_id = paste0(feature_set, "_", ts_length))

sinusoid <- grab_comp_times(R_path = "output/comptime/sinusoid/R.csv",
                            tsfresh_path = "output/comptime/sinusoid/tsfresh.csv",
                            TSFEL_path = "output/comptime/sinusoid/tsfel.csv",
                            Kats_path = "output/comptime/sinusoid/kats.csv",
                            hctsa_path = "output/comptime/sinusoid/outputTimes.csv") %>%
  mutate(unique_id = paste0(feature_set, "_", ts_length))

#--------------- Checking consistency of times ------------

# Graphical check

data.frame(mean_gaussian = gaussian$mean,
                      mean_sinusoid = sinusoid$mean,
                      ts_length = gaussian$ts_length,
                      feature_set = gaussian$feature_set,
                      feature_set_feats = gaussian$feature_set_feats) %>%
  ggplot(aes(x = mean_gaussian, y = mean_sinusoid, colour = feature_set)) +
  geom_point(size = 1.5) +
  labs(x = "Gaussian computation time",
       y = "Noisy sinusoid computation time",
       colour = NULL) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(labels = trans_format("log10", label_math())) +
  scale_y_log10(labels = trans_format("log10", label_math())) +
  theme_bw() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 18))

# Numerical check with Bonferroni correction

storage <- list()
unique_ids <- unique(gaussian$unique_id)

for(i in unique_ids){
  
  tmpGauss <- gaussian %>%
    filter(unique_id == i)
  
  tmpSine <- sinusoid %>%
    filter(unique_id == i)
  
  outs <- t.test(tmpGauss$mean, tmpSine$mean)
  
  tmp <- data.frame(unique_id = i,
                    pVal = outs$p.value)
  
  storage[[i]] <- tmp
}

pVals <- rbindlist(storage, use.names = TRUE) %>%
  mutate(indicator = ifelse(pVal < 0.05 / length(unique_ids), "Significant", "N.S."))

