# -*- coding: utf-8 -*-
"""
Created on Thu Oct 12 17:15:16 2017

@author: YSu
"""
import pandas as pd
import numpy as np
import random
import os
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as st
import statsmodels as sm
import seaborn as sns
from statistics import stdev
from scipy.stats import gamma
from scipy.stats import lognorm
from pandas.plotting import autocorrelation_plot as pdacf

os.chdir('C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master')

eugP=pd.read_csv('Historical_weather_analysis/eugP.csv')
eugP1=pd.read_csv('Historical_weather_analysis/eugP_med.csv')
eugP2=pd.read_csv('Historical_weather_analysis/eugP_high.csv')

min0=np.min(eugP.loc[:,['V8']])-.01
min1=np.min(eugP1.loc[:,['detP']])-.01
min2=np.min(eugP2.loc[:,['detP']])-.01

detP0=eugP.loc[:,'V8']-float(min0)
detP1=eugP1.loc[:,'detP']-float(min1)
detP2=eugP2.loc[:,'detP']-float(min2)

eugP0=eugP.loc[:,'PRCP']

import warnings
import numpy as np
import pandas as pd
import scipy.stats as st

import matplotlib
import matplotlib.pyplot as plt
# Create models from data 
        
        
def best_fit_distribution(data, bins=200):
    """Model data by finding best fit distribution to data"""
    # Get histogram of original data
    y, x = np.histogram(data, bins=bins, density=True)
    x = (x + np.roll(x, -1))[:-1] / 2.0

    # Distributions to check
    DISTRIBUTIONS = [        
        
        st.dgamma,st.dweibull,st.expon,st.exponnorm,st.exponweib,st.exponpow,
        st.genpareto,st.genexpon,
        st.gamma,st.gengamma,
        st.invgamma,st.invgauss,
        st.invweibull,
        st.logistic,st.loggamma,st.lognorm,
        st.norm,st.pareto

    ]

    # Best holders
    best_distribution = st.norm
    best_params = (0.0, 1.0)
    best_sse = np.inf

    # Estimate distribution parameters from data
    for distribution in DISTRIBUTIONS:

        # Try to fit the distribution
        try:
            # Ignore warnings from data that can't be fit
            with warnings.catch_warnings():
                warnings.filterwarnings('ignore')

                # fit dist to data
                params = distribution.fit(data)

                # Separate parts of parameters
                arg = params[:-2]
                loc = params[-2]
                scale = params[-1]

                # Calculate fitted PDF and error with fit in distribution
                pdf = distribution.pdf(x, loc=loc, scale=scale, *arg)
                sse = np.sum(np.power(y - pdf, 2.0))



                # identify if this distribution is better
                if best_sse > sse > 0:
                    best_distribution = distribution
                    best_params = params
                    best_sse = sse

        except Exception:
            pass

    return (best_distribution.name, best_params)


params = st.invgauss.fit(detP0)
    # Separate parts of parameters
arg = params[:-2]
loc = params[-2]
scale = params[-1]
shape=1/(float(scale))
                # Calculate fitted PDF and error with fit in distribution
pdf = st.invgauss.pdf(detP0, loc=loc, scale=scale, *arg)
sse = np.sum(np.power(y - pdf, 2.0))

                # identify if this distribution is better   
                    
Jan=eugP[eugP['Month']==1]             
Jan2=Jan.loc[:,'PRCP']
                    
best_fit_distribution(Jan2,bins=200)
best_fit_distribution(detP0,bins=200)
best_fit_distribution(detP1,bins=200)
best_fit_distribution(detP2,bins=200)

import seaborn as sns
simp=st.exponnorm.rvs(scale,size=9653)
sns.distplot(detP0,color="purple")
sns.distplot(simp,color="orange")





















































