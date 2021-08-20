#--------------------------------------
# This script sets out to produce
# computation time benchmarking analysis
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 17 August 2021
#----------------------------------------

# Fix Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

#------------------ Benchmark calculations ----------

#-------------------
# Compute times
# for R packages
#-------------------



#-------------------
# Read in results
# for non-R packages
#-------------------



#-------------------
# Bind all together
# for plotting
#-------------------



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
