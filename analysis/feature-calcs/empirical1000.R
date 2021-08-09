#------------------------------------
# This script sets out to calculate
# time-series features for each
# Empirical 1000 dataset and retain
# them for analytical use
#
# NOTE: setup.R must be run first
#------------------------------------

#---------------------------------------
# Author: Trent Henderson, 9 August 2021
#---------------------------------------

# Read in data

load("data/empirical1000.Rda")

# Fix Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

#----------------- Calculate features -----------------

outs_22 <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "catch22")
outs_fe <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "feasts")
outs_ts <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "tsfeatures")
outs_tsfresh <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "tsfresh", tsfresh_cleanup = FALSE)
outs_tsfel <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "tsfel")
outs_kats <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", group_var = "Keywords", feature_set = "kats")
      
# Bind together

Emp1000FeatMat <- bind_rows(outs_22, outs_fe, outs_ts, outs_tsfresh, outs_tsfel, outs_kats)

# Save as .Rda

save(Emp1000FeatMat, file = "data/Emp1000FeatMat.Rda")

# Clean up environment

rm(empirical1000, outs_22, outs_fe, outs_ts, outs_tsfresh, outs_tsfel, outs_kats, Emp1000FeatMat)
