#--------------------------------------
# This script sets out to plot a 
# random set of time series for
# demonstration in the report
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 18 August 2021
#----------------------------------------

# Read in data

load("data/empirical1000.Rda")

# Get a random sample of 6 time series

set.seed(1234)
theids <- unique(empirical1000$id)
sampleIDs <- sample(theids, 6)

mySamples <- empirical1000 %>%
  filter(id %in% sampleIDs)

# Draw plot

p <- mySamples %>%
  ggplot(aes(x = timepoint, y = value, colour = Keywords)) +
  geom_line() +
  labs(x = "Time",
       y = "Value",
       colour = NULL) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~Name, scales = "free", ncol = 2)

print(p)

ggsave("output/sample-ts.png", p)
