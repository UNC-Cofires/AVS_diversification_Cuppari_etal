# -*- coding: utf-8 -*-
"""
Created on Tue Jul 23 18:00:37 2019

@author: rcuppari
"""

##########looking at rain states given *simulated* tmax/tmin instead of actual
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
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from scipy.stats import lognorm
from sklearn.neighbors import KNeighborsRegressor

##historical data
os.chdir('C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master')

##need to input awnd/ghi/month for tmax and tmin in historical data 
detWeath=pd.read_csv('Historical_weather_analysis/eugWTIdet.csv')

##want KNeighborsRegressor and choose the #k 
tmin=detWeath.loc[:,['TMIN.1']]
tmax=detWeath.loc[:,['TMAX.1']]
weather=detWeath.loc[:,['AWND.1','GHI.1','TAVG.1']]

scaler=StandardScaler()
scaler.fit(weather)
weather=scaler.transform(weather)

end=len(detWeath)
split=round(int(.8*end))

max_train=tmax.iloc[0:split,:]
max_test=tmax.iloc[split:end,:]

min_train=tmin.iloc[0:split,:]
min_test=tmin.iloc[split:end,:]

weath_train=weather[0:split,:]
weath_test=weather[split:end,:]

day2=detWeath.iloc[split:end,:]

max_train=np.array(max_train)
max_train=max_train.reshape(-1,1)

max_test=np.array(max_test)
max_test=max_test.reshape(-1,1)

min_train=np.array(min_train)
min_train=min_train.reshape(-1,1)

min_test=np.array(min_test)
min_test=min_test.reshape(-1,1)

weath_train=np.array(weath_train)
weath_test=np.array(weath_test)

##train the model
knn=KNeighborsRegressor(n_neighbors=10,weights='distance',algorithm='auto')

knnMax=knn.fit(weath_train,max_train)

knnMin=knn.fit(weath_train,min_train)

##use model on all data 
predMin=knnMin.predict(weather)
predMax=knnMax.predict(weather)

predMax=pd.DataFrame(predMax)
predMin=pd.DataFrame(predMin)

combMax=pd.concat([predMax,detWeath],axis=1)
combMin=pd.concat([predMin,detWeath],axis=1)

synvalMax=np.zeros(len(combMax))
synvalMax=pd.DataFrame(synvalMax)
synvalMin=np.zeros(len(combMin))
synvalMin=pd.DataFrame(synvalMin)

for i in range(0,len(combMax)):
    synvalMax.iloc[i,0]=combMax.iloc[i,0]*combMax.iloc[i,19]+combMax.iloc[i,18]

for i in range(0,len(combMin)):
    synvalMin.iloc[i,0]=combMin.iloc[i,0]*combMin.iloc[i,21]+combMin.iloc[i,20]

plt.plot(synvalMax.iloc[:,0])
plt.plot(synvalMin.iloc[:,0])
plt.plot(combMin.loc[:,['TMAX']])
plt.plot(combMin.loc[:,['TMIN']])

synvalMax.columns=['simMax']
synvalMin.columns=['simMin']

##once simulate tmax/tmin need to plug into logreg 
eugWTI=pd.concat([synvalMax.reset_index(),detWeath.reset_index()],axis=1)
eugWTI=pd.concat([synvalMin.reset_index(),eugWTI],axis=1)

eugWTI=eugWTI.dropna()

length=round((.8*len(eugWTI)))
train=eugWTI.iloc[1:length,:]
end=len(eugWTI)
test=eugWTI.iloc[length:end,:]

trainFeat=train.loc[:,['AWND','TMAX','TMIN','V12','Month']] ##before had GHI instead of Month##
trainState=np.ravel(train.loc[:,['rain']])
testFeat=test.loc[:,['AWND','simMax','simMin','V12','Month']]
testState=np.ravel(test.loc[:,['rain']])

logreg=LogisticRegression()
logreg.fit(trainFeat,trainState)

##predict test data and compare 
predict=logreg.predict(testFeat)
cnf_mat=metrics.confusion_matrix(testState,predict)
cnf_mat

print("Accuracy:",metrics.accuracy_score(testState,predict))
print("Precision:",metrics.precision_score(testState,predict))


alltest=eugWTI.loc[:,['AWND','simMax','simMin','V12','Month']]
full_pred=logreg.predict(alltest)
cnf_mat_full=metrics.confusion_matrix(eugWTI.loc[:,'rain'],full_pred)
cnf_mat_full

alltest_state=eugWTI.loc[:,['rain']]
print("Accuracy:",metrics.accuracy_score(alltest_state,full_pred))
print("Precision:",metrics.precision_score(alltest_state,full_pred))


##once state is determined, generate random sample of rain amount (doing just for test sample now)  
##use all historical data on precipitation
test=pd.DataFrame(test)
predict=pd.DataFrame(predict) 
simRain=pd.concat([test.reset_index(),predict.reset_index()],axis=1)
simRain.to_csv('rainStates_validation_synT.csv')

eugWTI=pd.DataFrame(eugWTI)
full_pred=pd.DataFrame(full_pred)
simRainFull=pd.concat([eugWTI.reset_index(),full_pred.reset_index()],axis=1)
simRainFull.to_csv('rainStates_synT.csv')


















