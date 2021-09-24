#----------------------------------------
# This script sets out to run TSFEL
# and track time to compute a set of
# Gaussian noise of different lengths
#----------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 24 September 2021
#-------------------------------------------

import glob
import os
import time
import pandas as pd
import numpy as np
import tsfel

#------------ Run benchmarking -------------

os.chdir("/Users/trenthenderson/Documents/Git/feature-set-comp/data/sinusoid/sims")

cols = ['ts_length', 'mean', 'feature_set']
lst = []

for file in glob.glob("*.csv"):

    # Read in file

    x = pd.read_csv(file)
    x = x.loc[:,"values"]

    # Run computation

    if __name__ == '__main__': 
        cfg_file = tsfel.get_features_by_domain()
        start = time.time()
        extracted_features = tsfel.time_series_features_extractor(cfg_file, x)
        stop = time.time()
        duration = stop - start
        lst.append([x.shape[0], duration, "TSFEL"])

# Get results in clean tidy format

results = pd.DataFrame(lst, columns = cols)

# Store outputs

results.to_csv("/Users/trenthenderson/Documents/Git/feature-set-comp/output/comptime/sinusoid/tsfel.csv", index = False)
