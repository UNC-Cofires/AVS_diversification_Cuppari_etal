# -*- coding: utf-8 -*-
"""
Created on Fri Jul 19 16:09:03 2019

@author: rcuppari
"""

##################precipitation simulation#####################
import pandas as pd
import numpy as np
import random
import os
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
import matplotlib.pyplot as plt
import scipy
import scipy.stats as st
import seaborn as sns

##historical data
os.chdir('C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research')
weather=pd.read_csv('Salem_weather_fulldet.csv')
weather=weather.dropna(subset=['TMAX','GHI','AWND'])

##this should certainly be laid out in any easier way for it to be a functional long term model
##have inputs as the probabilities and then the individual weather things necessary 
weather=weather.loc[:,['AWND','PRCP','TAVG','TMAX','TMIN','Year','Month','Day','GHI',
                       'Clearsky','wind','RH','temp','daynum','maxCS','CloudCov','rain1','pww','pwv',
                       'pvv','pvw','pdw','pdv','rain']]


length=round((.8*len(weather)))
train=weather.iloc[1:length,:]
end=len(weather)
test=weather.iloc[length:end,:]

trainFeat=train.loc[:,['TMAX','Month','AWND','CloudCov']]
trainState=np.ravel(train.loc[:,['rain1']])
testFeat=test.loc[:,['TMAX','Month','AWND','CloudCov']]
testState=np.ravel(test.loc[:,['rain1']])

logreg=LogisticRegression(max_iter=10000,random_state=0,multi_class='multinomial',solver='newton-cg')
logreg.fit(trainFeat,trainState)

##predict test data and compare 
predict=logreg.predict(testFeat)
cnf_mat=metrics.confusion_matrix(testState,predict)
cnf_mat

print("Accuracy:",metrics.accuracy_score(testState,predict))

alltest=weather.loc[:,['TMAX','Month','AWND','CloudCov']]
full_pred=logreg.predict(alltest)
cnf_mat_full=metrics.confusion_matrix(weather.loc[:,'rain1'],full_pred)
cnf_mat_full

print("Accuracy:",metrics.accuracy_score(weather.loc[:,'rain1'],full_pred))

##once wet or dry state is determined, randomly sample from empirically 
##derived discrete distribution to decide wet or very wet 
full_pred=pd.DataFrame(full_pred)
simRain=pd.concat([weather.reset_index(),full_pred],axis=1)
simRain.rename(columns={'0':'simRainType1'},inplace=True)

simRain['simRainType']=np.zeros(shape=(len(simRain),1))
simRain.iloc[0,26]=1
for i in range(1,len(simRain)): 
    if simRain.iloc[i,25]==0:
        simRain.iloc[i,26]=0
    elif simRain.iloc[(i-1),26]==0 and simRain.iloc[i,25]==1: 
        x=np.random.uniform(0,1,1)
        if x <= simRain.iloc[i,22]:
            simRain.iloc[i,26]=1
        else: 
            simRain.iloc[i,26]=2
    elif simRain.iloc[(i-1),26]==2 and simRain.iloc[i,25]==1: 
        x=np.random.uniform(0,1,1)
        if x <= simRain.iloc[i,21]:
            simRain.iloc[i,26]=1
        else: 
            simRain.iloc[i,26]=2
    elif simRain.iloc[(i-1),26]==1 and simRain.iloc[i,25]==1: 
        x=np.random.uniform(0,1,1)
        if x <= simRain.iloc[i,18]:
            simRain.iloc[i,26]=1
        else: 
            simRain.iloc[i,26]=2
            
ax=sns.distplot(simRain['simRainType'],label="predicted") 
sns.distplot(simRain['rain'],label="observed")
ax.set_title('Histogram of Simulated v Observed Rain States')
ax.set_ylabel('count')
ax.set_xlabel('state')
plt.legend()

simRain.to_csv('simRainSal.csv')

predicted=pd.DataFrame(simRain.loc[:,'simRainType'])
predicted.to_csv('predictedSal.csv')
actual=pd.DataFrame(simRain.loc[:,'rain'])
actual.to_csv('actualSal.csv')

##then generate random sample of rain amount (doing just for test sample now)  
##use all historical data on precipitation
test=pd.DataFrame(test)
predict=pd.DataFrame(predict) 
simRain=pd.concat([test.reset_index(),predict.reset_index()],axis=1)

simRain=simRain.iloc[:,[2,3,13,5,7,6,8,9,10,11,12,15]]

##################################################################
##################################################################
##this whole part needs to be redone with Salem data and in a way 
##that it actually works :) 
##################################################################
##################################################################

##need to sample from different distributions for different months 
eugP1=pd.read_csv('Historical_weather_analysis/corvPdet.csv')
eugP1=eugP1[eugP1['rain']==2]
#eugP1.loc[:,'detP']=eugP1.loc[:,'detP']+1.56

Jan=eugP1[eugP1['Month']==1]
Feb=eugP1[eugP1['Month']==2]
Mar=eugP1[eugP1['Month']==3]
Apr=eugP1[eugP1['Month']==4]
May=eugP1[eugP1['Month']==5]
Jun=eugP1[eugP1['Month']==6]
Jul=eugP1[eugP1['Month']==7]
Aug=eugP1[eugP1['Month']==8]
Sep=eugP1[eugP1['Month']==9]
Oct=eugP1[eugP1['Month']==10]
Nov=eugP1[eugP1['Month']==11]
Dec=eugP1[eugP1['Month']==12]

pJan=Jan.loc[:,['PRCP']]
pFeb=Feb.loc[:,['PRCP']]
pMar=Mar.loc[:,['PRCP']]
pApr=Apr.loc[:,['PRCP']]
pMay=May.loc[:,['PRCP']]
pJun=Jun.loc[:,['PRCP']]
pJul=Jul.loc[:,['PRCP']]
pAug=Aug.loc[:,['PRCP']]
pSep=Sep.loc[:,['PRCP']]
pOct=Oct.loc[:,['PRCP']]
pNov=Nov.loc[:,['PRCP']]
pDec=Dec.loc[:,['PRCP']]

##fit different distributions by month## 
paramsJan=st.gamma.fit(pJan) 
paramsFeb=st.gamma.fit(pFeb) 
paramsMar=st.gamma.fit(pMar)
paramsApr=st.gamma.fit(pApr)
paramsMay=st.gamma.fit(pMay)
paramsJun=st.gamma.fit(pJun)
paramsJul=st.gamma.fit(pJul)
paramsAug=st.gamma.fit(pAug)
paramsSep=st.gamma.fit(pSep)
paramsOct=st.gamma.fit(pOct)
paramsNov=st.gamma.fit(pNov)
paramsDec=st.gamma.fit(pDec)
################gamma###############

eugP1['simP']=np.zeros((len(eugP1),1))
eugP1=pd.DataFrame(eugP1)

for i in range(0,len(eugP1)):
        if eugP1.iloc[i,5]==1:
       
            eugP1.iloc[i,12]=np.random.gamma((paramsJan[0]),paramsJan[-1])
    
        elif eugP1.iloc[i,5]==2:
        
            eugP1.iloc[i,12]=np.random.gamma((paramsFeb[0]),(paramsFeb[-1]))
        
        elif eugP1.iloc[i,5]==3: 
       
            eugP1.iloc[i,12]=np.random.gamma(paramsMar[0],(paramsMar[-1]))
        
        elif eugP1.iloc[i,5]==4:
            
            eugP1.iloc[i,12]=np.random.gamma(paramsApr[0],(paramsApr[-1]))
            
        elif eugP1.iloc[i,5]==5:
       
           eugP1.iloc[i,12]=np.random.gamma(paramsMay[0],(paramsMay[-1]))
        
        elif eugP1.iloc[i,5]==6:
             
            eugP1.iloc[i,12]=np.random.gamma(paramsJun[0],(paramsJun[-1]))
        
        elif eugP1.iloc[i,5]==7: 
           
            eugP1.iloc[i,12]=np.random.gamma(paramsJul[0],(paramsJul[-1]))
        
        elif eugP1.iloc[i,5]==8:
           
            eugP1.iloc[i,12]=np.random.gamma(paramsAug[0],(paramsAug[-1]))
        
        elif eugP1.iloc[i,5]==9:
 
            eugP1.iloc[i,12]=np.random.gamma(paramsSep[0],(paramsSep[-1]))
        
        elif eugP1.iloc[i,5]==10: 
  
            eugP1.iloc[i,12]=np.random.gamma(paramsOct[0],(paramsOct[-1]))
        
        elif eugP1.iloc[i,5]==11:
   
            eugP1.iloc[i,12]=np.random.gamma(paramsNov[0],(paramsNov[-1]))
        
        else: ##december 
     
            eugP1.iloc[i,12]=np.random.gamma(paramsDec[0],(paramsDec[-1]))

#eugP1['simP2']=np.zeros((len(eugP1),1))
#eugP1=pd.DataFrame(eugP1)

#for i in range(1,len(eugP1)): 
#    eugP1.iloc[i,13]=((eugP1.iloc[i,12]-1.55)*eugP1.iloc[i,9])+eugP1.iloc[i,8]
#    if eugP1.iloc[i,13]<0.03: 
#        eugP1.iloc[i,13]=np.random.uniform(.03,.1,1)

synval=eugP1.iloc[:,12]
obsval=eugP1.loc[:,'PRCP']

plt.hist([synval,obsval],bins=100,density=True,
         color=['red','darkblue'],label=['simulated','predicted'])
plt.xlabel('in rainfall')
plt.ylabel('frequency')
plt.title('Side by Side histogram of Simulated v. Observed Values')
#plt.savefig('sideBySideRain.png')

eugP1.to_csv('simRain_gamma_Corv_det.csv')
