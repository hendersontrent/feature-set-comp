#----------------------------------------
# This script sets out to run tsfresh
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
from tsfresh import extract_features

#------------ Run benchmarking -------------

os.chdir("/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims")

cols = ['ts_length', 'mean', 'feature_set']
lst = []

for file in glob.glob("*.csv"):

    x = pd.read_csv(file)
    start = time.time()
    extracted_features = extract_features(x, column_id = column_id, column_sort = column_sort)
    stop = time.time()
    duration = stop - start
    lst.append([x.shape[0], duration, "tsfresh"])

results = pd.DataFrame(lst, columns = cols)

# Store outputs

results.to_csv("/Users/trenthenderson/Documents/Git/feature-set-comp/output/comptime/tsfresh.csv", index = False)
