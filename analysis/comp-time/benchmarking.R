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

# Define a helper function

#' Function to compute benchmarks
#' 
#' @param my_set the feature set to calculate
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

run_benchmark <- function(my_set){
  
  set.seed(123)
  ids <- unique(empirical1000$id)
  storage <- list()
  
  # Random one off to generate burn-in
  
  short_id <- ids[1]
  
  shorter <- empirical1000 %>% 
    filter(id == short_id)
  
  calculate_features(shorter, id_var = "id", 
                     time_var = "timepoint", 
                     values_var = "value", 
                     group_var = "Keywords", 
                     feature_set = my_set)

  for(i in ids){
    
    message(paste0("Computing features for ", i))
    
    tryCatch({
    
      tmp <- empirical1000 %>% 
        filter(id == i)
      
      m <- summary(microbenchmark(calculate_features(tmp, 
                                                     id_var = "id", 
                                                     time_var = "timepoint", 
                                                     values_var = "value", 
                                                     group_var = NULL, 
                                                     feature_set = my_set), 
                                  times = 1, unit = "s"))
      
      mdat <- data.frame(m) %>%
        dplyr::select(c(mean)) %>%
        mutate(id = i)
    
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    storage[[i]] <- mdat
  }
  
  outs <- data.table::rbindlist(storage, use.names = TRUE) %>%
    mutate(feature_set = my_set)
  
  return(outs)
}

# Run benchmarking

outs_22 <- run_benchmark(my_set = "catch22")
outs_feasts <- run_benchmark(my_set = "feasts")
outs_tsfeatures <- run_benchmark(my_set = "tsfeatures")
outs_kats <- run_benchmark(my_set = "kats")
outs_tsfresh <- run_benchmark(my_set = "tsfresh")
outs_tsfel <- run_benchmark(my_set = "tsfel")

all_features <- bind_rows(outs_22, outs_feasts, outs_tsfeatures, outs_kats, outs_tsfresh, outs_tsfel)

# Join in length labels

length_labels <- empirical1000 %>%
  group_by(id) %>%
  summarise(length = max(timepoint)) %>%
  ungroup()

all_features <- all_features %>%
  left_join(length_labels, by = c("id" = "id")) %>%
  group_by(feature_set, length) %>%
  summarise(mean = mean(mean),
            min = min(mean),
            max = max(mean)) %>%
  ungroup()

#------------------ Graphical summary ---------------

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
