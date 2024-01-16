# -*- coding: utf-8 -*-
"""
Created on Sat Jul 27 19:52:22 2019

@author: rcuppari
"""
##################precipitation simulation using lognormal#####################
import pandas as pd
import numpy as np
import random
import os
from sklearn import metrics
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as st
import statsmodels as sm
from statistics import stdev
from pandas.plotting import autocorrelation_plot as pdacf
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import KNeighborsClassifier
from statsmodels.tsa.api import VAR, DynamicVAR

##historical data
os.chdir('C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master')
eugWTI=pd.read_csv('Historical_weather_analysis/SalDet2s1997lap.csv')

eugWTI=eugWTI.dropna(subset=['AWND','GHI','rain'])
weather=eugWTI.loc[:,['TMAX','GHI','AWND','Month','lag']]

scaler=StandardScaler()
scale_eugWTI=pd.DataFrame(scaler.fit_transform(eugWTI.loc[:,['Month','AWND','GHI','TMAX']]))

length=round((.8*len(eugWTI)))
train=scale_eugWTI.iloc[0:length,:]
end=len(eugWTI)
test=scale_eugWTI.iloc[length:end,:]

end=len(eugWTI)
split=round(int(.8*end))

p_train=eugWTI.iloc[0:length,21]
p_test=eugWTI.iloc[length:end,21]

knn=KNeighborsClassifier(n_neighbors=19,algorithm='auto')
knn=knn.fit(train,p_train)
pred=knn.predict(test)


metrics.r2_score(pred,p_test)
print("Accuracy:",metrics.accuracy_score(pred,p_test))

sns.distplot(p_test,norm_hist=True,kde=True,color='darkblue')
sns.distplot(pred,norm_hist=True,kde=True,color='red')

test=eugWTI.iloc[split:end,:]
##whole dataset
predTot=knn.predict(scale_eugWTI)
prcp=eugWTI.iloc[:,32]
sns.distplot(prcp,norm_hist=True,kde=True,color='darkblue')
sns.distplot(predTot,norm_hist=True,kde=True,color='red')
metrics.r2_score(predTot,prcp)
print("Accuracy:",metrics.accuracy_score(predTot,prcp))

full_pred=pd.DataFrame(predTot)
simRain=pd.concat([eugWTI.reset_index(),full_pred],axis=1)
simRain.rename(columns={'0':'simRainType1'},inplace=True)
simRain['simRainType']=np.zeros(shape=(len(simRain),1))

for i in range(0,len(simRain)): 
    if simRain.iloc[i,34]==0:
        simRain.iloc[i,35]=0
    else: 
        x=np.random.uniform(0,1,1)
        if x <= simRain.iloc[i,31]:
            simRain.iloc[i,35]=1
        else: 
            simRain.iloc[i,35]=2

ax=sns.distplot(simRain['simRainType'],label="predicted") 
sns.distplot(simRain['rain'],label="observed")
ax.set_title('Histogram of Simulated v Observed Rain States')
ax.set_ylabel('count')
ax.set_xlabel('state')
plt.legend()

simRain.to_csv('simRainSal_1997_knnmon.csv')

predicted=pd.DataFrame(simRain.loc[:,'simRainType'])
predicted.to_csv('predictedStateSal_1997_knnmon.csv')
actual=pd.DataFrame(simRain.loc[:,'rain'])
actual.to_csv('actualSal_1997_knnmon.csv')

####################################################
pdacf(pred,color="orange",label="simulated")
pdacf(p_test,color="purple",label="observed")
plt.legend()
plt.title('Simulated v. Observed Rainfall PACF')
plt.savefig('pacf_rainfall_KNN2_uni.png')


###################################################
predTot=pd.DataFrame(predTot)
tot=pd.concat([predTot,prcp.reset_index()],axis=1)
tot.to_csv('wholeSet.csv')

error=[]
for i in range(1,100):
    knn=KNeighborsClassifier(n_neighbors=i)
    knn.fit(train,p_train)
    pred_i=knn.predict(test)
    error.append(np.mean(pred_i!=p_test))
    
plt.figure(figsize=(12,6))
plt.plot(range(1,100),error,color='red',linestyle='dashed',marker='o',
         markerfacecolor='blue',markersize=10)
plt.title('Error Rate K Value')
plt.xlabel('K Value')
plt.ylabel('Mean Error')


#####################for detrended data######################################
eugWTI=eugWTI.dropna()
eugWeath=eugWTI.loc[:,['TMIN.1','TMAX.1','GHI.1']]

scaler=StandardScaler()
scaler.fit(eugWeath)
weather=scaler.transform(eugWeath)

lag=eugWTI.loc[:,['V30']]
lag=pd.DataFrame(lag)
weather=pd.DataFrame(weather)
weather=pd.concat([weather,lag.reset_index()],axis=1)

weather=weather.iloc[:,[0,1,2,4]]
end=len(eugWTI)
split=round(int(.8*end))

p_train=eugWTI.iloc[0:split,25]
p_test=eugWTI.iloc[split:end,25]

weather=pd.DataFrame(weather)
weath_train=weather.iloc[0:split,:]
weath_test=weather.iloc[split:end,:]
test=eugWTI.iloc[split:end,:]

knn=KNeighborsRegressor(n_neighbors=2,weights='distance',algorithm='auto')
knn=knn.fit(weath_train,p_train)
pred=knn.predict(weath_test)

r2=metrics.r2_score(pred,p_test)
r2

sns.distplot(p_test,norm_hist=True,kde=True,color='darkblue')
sns.distplot(pred,norm_hist=True,kde=True,color='red')

synval=np.zeros(len(pred))
synval=pd.DataFrame(synval)
pred=pd.DataFrame(pred)
for i in range(0,len(pred)):
    synval.iloc[i,0]=pred.iloc[i,0]*test.iloc[i,15]+test.iloc[i,14]

test2=test.reset_index()
plt.plot(synval,color="red")
plt.plot(test2.iloc[:,2])


















