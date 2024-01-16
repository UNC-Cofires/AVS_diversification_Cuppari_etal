# -*- coding: utf-8 -*-
"""
Created on Wed Jul 17 12:02:04 2019

@author: rcuppari
"""
import pandas as pd
import numpy as np
import random
import os
from sklearn import metrics
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.neighbors import KNeighborsRegressor
import scipy
import scipy.stats as st
import sklearn.neighbors
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split as tts

detWeath=pd.read_csv('Historical_weather_analysis/SalDet2s1997lap.csv')
detWeath=detWeath.dropna(subset=['TMIN.1','TMAX.1','GHI.1','AWND.1'])

##want KNeighborsRegressor and choose the #k 
tmin=detWeath.loc[:,['TMIN.1']]
tmax=detWeath.loc[:,['TMAX.1']]
weather=detWeath.loc[:,['AWND.1','GHI.1','TAVG.1']]

end=len(detWeath)
split=round(int(.8*end))

max_train=tmax.iloc[0:split,:]
max_test=tmax.iloc[split:end,:]

min_train=tmin.iloc[0:split,:]
min_test=tmin.iloc[split:end,:]

weath_train=weather.iloc[0:split,:]
weath_test=weather.iloc[split:end,:]

weath2_train=weather.iloc[0:split,:]
weath2_test=weather.iloc[split:end,:]

day1=detWeath.iloc[0:split,:]
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

weath2_train=np.array(weath2_train)
weath2_test=np.array(weath2_test)

##train the model
knn=KNeighborsRegressor(n_neighbors=10,weights='distance',algorithm='auto')

knnMax=knn.fit(weath_train,max_train)
predMax=knnMax.predict(weath_test)

knnMin=knn.fit(weath2_train,min_train)
predMin=knnMin.predict(weath2_test)

scoreMax=metrics.mean_squared_error(predMax,max_test)
scoreMax
scoreMin=metrics.mean_squared_error(predMin,min_test)
scoreMin

metrics.r2_score(predMax,max_test)
metrics.r2_score(predMin,min_test)

##re-season##
predMax=pd.DataFrame(predMax)
predMin=pd.DataFrame(predMin)
day2=pd.DataFrame(day2)
day2=day2.reset_index()
combMax=pd.concat([predMax,day2],axis=1)
combMin=pd.concat([predMin,day2],axis=1)

synvalMax=np.zeros(len(combMax))
synvalMax=pd.DataFrame(synvalMax)
synvalMin=np.zeros(len(combMin))
synvalMin=pd.DataFrame(synvalMin)

for i in range(0,len(combMax)):
    synvalMax.iloc[i,0]=combMax.iloc[i,0]*combMax.iloc[i,5]+combMax.iloc[i,4]

for i in range(0,len(combMin)):
    synvalMin.iloc[i,0]=combMin.iloc[i,0]*combMin.iloc[i,7]+combMin.iloc[i,6]

maxTestVals=combMax.loc[:,'TMAX']
minTestVals=combMin.loc[:,'TMIN']   
round(metrics.r2_score(synvalMax,maxTestVals),4)
round(metrics.r2_score(synvalMin,minTestVals),4)

################whole thing####################
##need to detrend 1200 year input using our trends 
syn_series=pd.read_csv('C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Synthetic_results\\Results\\synthetic_weather_data1200.csv')
syn_irrad=pd.read_csv('C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Synthetic_results\\Results\\synthetic_irradiance_data1200.csv')

predMinAll=knnMin.predict(weather)
predMaxAll=knnMax.predict(weather)

predMaxAll=pd.DataFrame(predMaxAll)
predMinAll=pd.DataFrame(predMinAll)

detWeath['simMax']=np.zeros(shape=(len(detWeath),1))
detWeath['simMin']=np.zeros(shape=(len(detWeath),1))

for i in range(0,len(detWeath)):
    detWeath.iloc[i,33]=predMaxAll.iloc[i,0]*detWeath.iloc[i,3]+detWeath.iloc[i,2]
    detWeath.iloc[i,34]=predMinAll.iloc[i,0]*detWeath.iloc[i,5]+detWeath.iloc[i,4]

#plt.plot(detWeath.loc[:,'TMAX'])
#plt.plot(detWeath.loc[:,'simMax'])

#plt.plot(detWeath.loc[:,'TMIN'])
#plt.plot(detWeath.loc[:,'simMin'])

round(metrics.r2_score(detWeath.loc[:,'simMax'],detWeath.loc[:,'TMAX']),4)
round(metrics.r2_score(detWeath.loc[:,'simMin'],detWeath.loc[:,'TMIN']),4)

detWeath.to_csv('simTempSal3.csv')


################PLOTS##################
fig1, ax=plt.subplots(figsize=(3,3))
ax.plot(synvalMax, 'red', label='simulated',alpha=0.8)
ax.plot(maxTestVals, 'darkblue', label='actual',alpha=0.5)
plt.title('Predicted and Actual TMAX (test data)')
plt.xlabel('days')
plt.ylabel('temperature (F)')
plt.savefig('sim_actual_comp_TMAX.png',dpi=500)

minTestVals=combMin.loc[:,'TMIN']
fig2, ax=plt.subplots(figsize=(3,3))
ax.plot(synvalMin, 'orange', label='simulated',alpha=0.8)
ax.plot(minTestVals, 'purple', label='actual',alpha=0.5)
plt.title('Predicted and Actual TMIN (test data)')
plt.xlabel('days')
plt.ylabel('temperature (F)')
plt.savefig('sim_actual_comp_TMIN.png',dpi=500)
    
sns.distplot(synvalMin)
sns.distplot(minTestVals)
plt.title('Tmin observed v predicted histogram')
plt.savefig('tminPredHist.png')

sns.distplot(synvalMax)
sns.distplot(maxTestVals)
plt.title('Tmax observed v predicted histogram')
plt.savefig('tmaxPredHist.png')

plt.scatter(synvalMax,maxTestVals,color='orange',alpha=.7)
x=np.linspace(0,100,100)
plt.plot(x,x)
plt.xlabel('predicted TMAX (F)')
plt.ylabel('observed TMAX (F)')
plt.title('Predicted v Observed TMAX Values')
plt.savefig('pred_obs_scat_tmax.png')

plt.scatter(synvalMin,minTestVals,color='blue',alpha=.5)
x=np.linspace(0,100,100)
plt.plot(x,x)
plt.xlabel('predicted TMIN (F)')
plt.ylabel('observed TMIN (F)')
plt.title('Predicted v Observed TMIN Values')
plt.savefig('pred_obs_scat_tmin.png')


####autocorrelation####
from pandas.plotting import autocorrelation_plot as pdacf
synvalMin2=synvalMin.iloc[0:365,:]
minTestVals2=minTestVals.iloc[0:365,:]

pdacf(synvalMin2,color="orange",label="simulated")
pdacf(minTestVals2,color="purple",label="observed")
plt.legend()
plt.title('Simulated v. Observed TMIN PACF')
plt.savefig('pacf_minT.png')

synvalMax2=synvalMax.iloc[0:365,:]
maxTestVals2=maxTestVals.iloc[0:365,]

pdacf(synvalMax2,color="orange",label="simulated")
pdacf(maxTestVals2,color="purple",label="observed")
plt.legend()
plt.title('Simulated v. Observed TMAX PACF')
plt.savefig('pacf_maxT.png')


####cross correlation####
import statsmodels.tsa.stattools as stattools

tmin=np.ravel(detWeath.loc[:,['TMIN']])
tmax=np.ravel(detWeath.loc[:,['TMAX']])
irrad=np.ravel(detWeath.loc[:,['GHI']])
prcp=np.ravel(detWeath.loc[:,['PRCP']])
wind=np.ravel(detWeath.loc[:,['AWND']])
tavg=np.ravel(detWeath.loc[:,['TAVG']])

##original cross correlations
min_max_cc=stattools.ccf(tmin,tmax)
min_irrad_cc=stattools.ccf(tmin,irrad)
min_wind_cc=stattools.ccf(tmin,wind)
min_tavg_cc=stattools.ccf(tmin,tavg)

nlags=len(min_max_cc)

plt.figure(figsize=(12,7),dpi=100)
plt.bar(x=np.arange(len(min_max_cc),min_max_cc,.3))

##new cross correlations
tmin2=np.ravel(day2.loc[:,['TMIN']])
tmax2=np.ravel(day2.loc[:,['TMAX']])
irrad2=np.ravel(day2.loc[:,['GHI']])
prcp2=np.ravel(day2.loc[:,['PRCP']])
wind2=np.ravel(day2.loc[:,['AWND']])
tavg2=np.ravel(day2.loc[:,['TAVG']])


min_max_ncc=stattools.ccf(synvalMin,tmax2)
min_irrad_ncc=stattools.ccf(synvalMin,irrad2)
min_wind_ncc=stattools.ccf(synvalMin,wind2)
min_tavg_ncc=stattools.ccf(synvalMin,tavg2)


###giant correlation plot###
synvalMin.columns=['synTMIN']
synvalMax.columns=['synTMAX']

combSyn=pd.concat([synvalMin,synvalMax,combMin.loc[:,['TMIN','TMAX','AWND','PRCP','GHI','TAVG']]],axis=1)
combSyn=combSyn.loc[:,['synTMIN','TMIN','synTMAX','TMAX','AWND','PRCP','GHI','TAVG']]
corrs=combSyn.corr()

cmap=sns.diverging_palette(255,10,as_cmap=True)

fig7,ax=plt.subplots(figsize=(11,9))
sns.heatmap(corrs,cmap=cmap,center=0,square=True)
plt.title('Correlation between Weather Variables')
plt.savefig('corrAll.png')

combMin2=combMin.loc[:,['TMIN','TMAX','AWND','PRCP','GHI','TAVG']]
combMinSyn=pd.concat([synvalMin,combMin.loc[:,['TMAX','AWND','PRCP','GHI','TAVG']]],axis=1)

combMax2=combMin.loc[:,['TMAX','TMIN','AWND','PRCP','GHI','TAVG']]
combMaxSyn=pd.concat([synvalMax,combMin.loc[:,['TMIN','AWND','PRCP','GHI','TAVG']]],axis=1)

corrMinOrg=combMin2.corr()
corrMaxOrg=combMax2.corr()
corrMinSyn=combMinSyn.corr()
corrMaxSyn=combMaxSyn.corr()

cmap=sns.diverging_palette(255,10,as_cmap=True)

fig5,ax=plt.subplots(figsize=(11,9))
sns.heatmap(corrMinOrg,cmap=cmap,center=0,square=True)
plt.title('Original Correlation between Weather Variables')
plt.savefig('corrMinOrg.png')

fig6,ax=plt.subplots(figsize=(11,9))
sns.heatmap(corrMinSyn,cmap=cmap,center=0,square=True)
plt.title('Correlation between Weather Variables and Synthetic TMIN')
plt.savefig('corrMinSyn.png')

fig6,ax=plt.subplots(figsize=(11,9))
sns.heatmap(corrMaxSyn,cmap=cmap,center=0,square=True)
plt.title('Correlation between Weather Variables and Synthetic TMAX')
plt.savefig('corrMaxSyn.png')







##############
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 17 12:02:04 2019

@author: rcuppari
"""
import pandas as pd
import numpy as np
import random
import os
from sklearn import metrics
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.neighbors import KNeighborsRegressor
import scipy
import scipy.stats as st
import sklearn.neighbors
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

os.chdir('C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master')
detWeath=pd.read_csv('Historical_weather_analysis/SalDet2s19973.csv')
detWeath=detWeath[detWeath['Year']>1997]
detWeath=detWeath.dropna(subset=['TMIN.1','TMAX.1','GHI.1','AWND.1'])

##want KNeighborsRegressor and choose the #k 
tmin=detWeath.loc[:,['TMIN.1']]
tmax=detWeath.loc[:,['TMAX.1']]
weather=detWeath.loc[:,['AWND.1','GHI.1','TAVG.1']]

end=len(detWeath)
split=round(int(.8*end))

max_train=tmax.iloc[0:split,:]
max_test=tmax.iloc[split:end,:]

min_train=tmin.iloc[0:split,:]
min_test=tmin.iloc[split:end,:]

weath_train=weather.iloc[0:split,:]
weath_test=weather.iloc[split:end,:]

weath2_train=weather.iloc[0:split,:]
weath2_test=weather.iloc[split:end,:]

day1=detWeath.iloc[0:split,:]
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

weath2_train=np.array(weath2_train)
weath2_test=np.array(weath2_test)

##train the model
knn=KNeighborsRegressor(n_neighbors=10,weights='distance',algorithm='auto') ##even though the graph shows otherwise, this seems to be the best fit

knnMax=knn.fit(weath_train,max_train)
predMax=knnMax.predict(weath_test)

knnMin=knn.fit(weath2_train,min_train)
predMin=knnMin.predict(weath2_test)

scoreMax=metrics.mean_squared_error(predMax,max_test)
scoreMax
scoreMin=metrics.mean_squared_error(predMin,min_test)
scoreMin

r2Max=metrics.r2_score(predMax,max_test)
r2Min=metrics.r2_score(predMin,min_test)
r2Max
r2Min

plt.plot(predMax)
plt.plot(max_test)

plt.plot(predMin)
plt.plot(min_test)

from sklearn.metrics import mean_squared_error 
from math import sqrt
rmse=[]
for K in range(1,30):
    K = K+1
    model = KNeighborsRegressor(n_neighbors = K)

    model.fit(weath_train, max_train)  #fit the model
    pred=model.predict(weath_test) #make prediction on test set
    error = sqrt(mean_squared_error(max_test,pred)) #calculate rmse
    rmse.append(error) #store rmse values
    
plt.figure(figsize=(12,6))
plt.plot(range(1,30),rmse,color='red',linestyle='dashed',marker='o',
         markerfacecolor='blue',markersize=10)
plt.title('Error Rate K Value')
plt.xlabel('K Value')
plt.ylabel('Mean Error')

rmse=[]
for K in range(1,30):
    K = K+1
    model = KNeighborsRegressor(n_neighbors = K)

    model.fit(weath_train, min_train)  #fit the model
    pred=model.predict(weath_test) #make prediction on test set
    error = sqrt(mean_squared_error(min_test,pred)) #calculate rmse
    rmse.append(error) #store rmse values
    
plt.figure(figsize=(12,6))
plt.plot(range(1,30),rmse,color='red',linestyle='dashed',marker='o',
         markerfacecolor='blue',markersize=10)
plt.title('Error Rate K Value')
plt.xlabel('K Value')
plt.ylabel('Mean Error')


##reason##
predMax=pd.DataFrame(predMax)
predMin=pd.DataFrame(predMin)
day2=pd.DataFrame(day2)
day2=day2.reset_index()
combMax=pd.concat([predMax,day2],axis=1)
combMin=pd.concat([predMin,day2],axis=1)

synvalMax=np.zeros(len(combMax))
synvalMax=pd.DataFrame(synvalMax)
synvalMin=np.zeros(len(combMin))
synvalMin=pd.DataFrame(synvalMin)

for i in range(0,len(combMax)):
    synvalMax.iloc[i,0]=combMax.iloc[i,0]*combMax.iloc[i,6]+combMax.iloc[i,5]


for i in range(0,len(combMin)):
    synvalMin.iloc[i,0]=combMin.iloc[i,0]*combMin.iloc[i,8]+combMin.iloc[i,7]

maxTestVals=combMax.loc[:,'TMAX']
minTestVals=combMin.loc[:,'TMIN']   
r2MaxRet=round(metrics.r2_score(synvalMax,maxTestVals),4)
r2MinRet=round(metrics.r2_score(synvalMin,minTestVals),4)
r2MaxRet
r2MinRet

################whole thing####################
predMinAll=knnMin.predict(weather)
predMaxAll=knnMax.predict(weather)

predMaxAll=pd.DataFrame(predMaxAll)
predMinAll=pd.DataFrame(predMinAll)

detWeath['simMax']=np.zeros(shape=(len(detWeath),1))
detWeath['simMin']=np.zeros(shape=(len(detWeath),1))

for i in range(0,len(detWeath)):
    detWeath.iloc[i,33]=predMaxAll.iloc[i,0]*detWeath.iloc[i,3]+detWeath.iloc[i,2]
    detWeath.iloc[i,34]=predMinAll.iloc[i,0]*detWeath.iloc[i,5]+detWeath.iloc[i,4]

plt.plot(detWeath.loc[:,'TMAX'])
plt.plot(detWeath.loc[:,'simMax'])

plt.plot(detWeath.loc[:,'TMIN'])
plt.plot(detWeath.loc[:,'simMin'])

round(metrics.r2_score(detWeath.iloc[:,33],detWeath.loc[:,'TMAX']),4)
round(metrics.r2_score(detWeath.iloc[:,34],detWeath.loc[:,'TMIN']),4)

detWeath.to_csv('simTempSal2.csv')



















































