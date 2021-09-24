#----------------------------------------
# This script sets out to run tsfresh
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
from tsfresh import extract_features

#------------ Run benchmarking -------------

os.chdir("/Users/trenthenderson/Documents/Git/feature-set-comp/data/sinusoid/sims")

cols = ['ts_length', 'mean', 'feature_set']
lst = []

for file in glob.glob("*.csv"):

    # Read in file

    timeseries = pd.read_csv(file)

    # Add in time and ID column for tsfresh

    timeseries['id'] = '1'
    timeseries['time'] = np.arange(len(timeseries))

    # Run computation

    if __name__ == '__main__': 
        start = time.time()
        extracted_features = extract_features(timeseries, column_id = 'id', column_sort = 'time')
        stop = time.time()
        duration = stop - start
        lst.append([timeseries.shape[0], duration, "tsfresh"])

# Get results in clean tidy format

results = pd.DataFrame(lst, columns = cols)

# Store outputs

results.to_csv("/Users/trenthenderson/Documents/Git/feature-set-comp/output/comptime/sinusoid/tsfresh.csv", index = False)
