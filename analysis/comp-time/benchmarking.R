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
kats_results <- readr::read_csv("output/comptime/kats.csv") #%>% filter(mean < 8)
tsfresh_results <- readr::read_csv("output/comptime/tsfresh.csv")
tsfel_results <- readr::read_csv("output/comptime/tsfel.csv") #%>% filter(mean < 0.20)

# Add vector of times to hctsa results

thelengths <- c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 
                250, 250, 250, 250, 250,250, 250, 250, 250, 250,
                500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 
                750, 750, 750, 750, 750, 750, 750, 750, 750, 750,
                1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000)

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
  mutate(feature_set_feats = case_when(
          feature_set == "catch22"    ~ "catch22 (22)",
          feature_set == "feasts"     ~ "feasts (43)",
          feature_set == "hctsa"      ~ "hctsa (7300)",
          feature_set == "Kats"       ~ "Kats (40)",
          feature_set == "tsfeatures" ~ "tsfeatures (62)",
          feature_set == "TSFEL"      ~ "TSFEL (285-390)",
          feature_set == "tsfresh"    ~ "tsfresh (779)"))

#------------------ Graphical summary ---------------

#---------
# Raw time
#---------

p <- all_comptimes %>%
  group_by(feature_set, feature_set_feats, ts_length) %>%
  summarise(avg = median(mean),
            sd = sd(mean)) %>%
  ungroup() %>%
  # mutate(lower = avg - (1*sd),
  #        upper = avg + (1*sd)) %>%
  ggplot() +
  geom_line(aes(x = ts_length, y = avg, colour = feature_set_feats)) +
  # geom_errorbar(aes(x = ts_length, y = avg, colour = feature_set_feats, ymin = lower, ymax = upper)) +
  geom_point(aes(x = ts_length, y = avg, colour = feature_set_feats), size = 2) +
  labs(subtitle = "A",
       x = "Time series length (samples)",
       y = "Computation time (s)",
       colour = NULL) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(breaks = c(1e2, 1e3),
                labels = trans_format("log10", label_math())) +
  scale_y_log10(limits = c(1e-3, 1e2),
                breaks = scales::trans_breaks("log10", function(x) 10^x, n = 6),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        plot.subtitle = element_text(face = "bold"))

print(p)

#-------
# Scaled
#-------

p1 <- all_comptimes %>%
  mutate(mean_scaled = case_when(
          feature_set == "catch22"                   ~ mean/22,
          feature_set == "feasts"                    ~ mean/43,
          feature_set == "hctsa"                     ~ mean/7300,
          feature_set == "Kats"                      ~ mean/40,
          feature_set == "tsfeatures"                ~ mean/22,
          feature_set == "tsfresh"                   ~ mean/779,
          feature_set == "TSFEL" & ts_length == 100  ~ mean/285,
          feature_set == "TSFEL" & ts_length == 250  ~ mean/285,
          feature_set == "TSFEL" & ts_length >= 500  ~ mean/390)) %>%
  group_by(feature_set, feature_set_feats, ts_length) %>%
  summarise(avg = median(mean_scaled),
            sd = sd(mean_scaled)) %>%
  ungroup() %>%
  # mutate(lower = avg - (1*sd),
  #        upper = avg + (1*sd)) %>%
  ggplot() +
  geom_line(aes(x = ts_length, y = avg, colour = feature_set_feats)) +
  # geom_errorbar(aes(x = ts_length, y = avg, colour = feature_set_feats, ymin = lower, ymax = upper)) +
  geom_point(aes(x = ts_length, y = avg, colour = feature_set_feats), size = 2) +
  labs(subtitle = "B",
       x = "Time series length (samples)",
       y = "Computation time per feature (s)",
       colour = NULL) +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_log10(limits = c(1e-5, 1e0),
                breaks = scales::trans_breaks("log10", function(x) 10^x, n = 6),
                labels = trans_format("log10", label_math())) +
  scale_x_log10(breaks = c(1e2, 1e3),
                labels = trans_format("log10", label_math())) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        plot.subtitle = element_text(face = "bold"))

print(p1)

# Save outputs

p2 <- ggpubr::ggarrange(p, p1, nrow = 2, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("output/comp-time-merged.png", p2, units = "in", height = 8.5, width = 10)
