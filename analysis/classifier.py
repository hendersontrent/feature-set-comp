#---------------------------------------
# This script defines a function to 
# calculate a machine learning model
# for classification of a dataset using
# time-series feature matrices as inputs
#---------------------------------------

#-------------------------------------
# Author: Trent Henderson, 13 May 2021
#-------------------------------------

import numpy as np
import pandas as pd
from sklearn.metrics import make_scorer, balanced_accuracy_score
from sklearn.svm import SVC

def fit_classifier(X_train, y_train, X_test, y_test):

    # Convert to numpy for sklearn

    X_train = np.array(X_train)
    y_train = np.array(y_train)
    X_test = np.array(X_test)
    y_test = np.array(y_test)

    # Fit classifier

    clf = SVC(kernel = 'linear', class_weight = 'balanced')
    clf.fit(X_train, y_train)

    # Predict on test set

    scores = clf.balanced_accuracy_score(X_test, y_test)

    # Return results

    data = [{'accuracy': scores}]
    df = pd.DataFrame(data)
    
    return df
