#----------------------------------------
# This script sets out to run Kats
# and track time to compute a set of
# Gaussian noise of different lengths
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 20 August 2021
#----------------------------------------

import glob
import os
import time
import pandas as pd
import numpy as np
from kats.consts import TimeSeriesData
from kats.tsfeatures.tsfeatures import TsFeatures

#------------ Run benchmarking -------------

os.chdir("/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/kats")

cols = ['ts_length', 'mean', 'feature_set']
lst = []

for file in glob.glob("*.csv"):

    # Read in file

    data = pd.read_csv(file)
    data['time'] = pd.to_datetime(data['time'])
    data['time'] = [x.date() for x in data.time]
    data = TimeSeriesData(data)

    # Instantiate TsFeatures

    model = TsFeatures()

    # Run computation

    if __name__ == '__main__': 
        start = time.time()
        extracted_features = model.transform(data)
        stop = time.time()
        duration = stop - start
        lst.append([data.shape[0], duration, "tsfresh"])

# Get results in clean tidy format

results = pd.DataFrame(lst, columns = cols)

# Store outputs

results.to_csv("/Users/trenthenderson/Documents/Git/feature-set-comp/output/comptime/kats.csv", index = False)
