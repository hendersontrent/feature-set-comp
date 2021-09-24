#----------------------------------------
# This script sets out to run Kats
# and track time to compute a set of
# sinusoids of different lengths
#----------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 24 September 2021
#-------------------------------------------

import glob
import os
import time
import pandas as pd
import numpy as np
from kats.consts import TimeSeriesData
from kats.tsfeatures.tsfeatures import TsFeatures

#------------ Run benchmarking -------------

os.chdir("/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/sinusoid/kats")

cols = ['ts_length', 'mean', 'feature_set']
lst = []

for file in glob.glob("*.csv"):

    # Read in file

    data1 = pd.read_csv(file)
    times = data1.loc[:,"time"]
    values = data1.loc[:,"value"]
    data = pd.DataFrame({'time':times, 'value':values})
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
        lst.append([data1.shape[0], duration, "Kats"])

# Get results in clean tidy format

results = pd.DataFrame(lst, columns = cols)

# Store outputs

results.to_csv("/Users/trenthenderson/Documents/Git/feature-set-comp/output/comptime/sinusoid/kats.csv", index = False)
