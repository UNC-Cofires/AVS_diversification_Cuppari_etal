# -*- coding: utf-8 -*-
"""
Created on Wed Aug 28 17:49:08 2019

@author: rcuppari
"""
import math
##############################################################################
##this script is intended to simulate precipitation given maximum temperature,
##irradiation, wind, and Month
##############################################################################

##historical data
weath=pd.read_csv('C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Salem_weather_fulldet.csv')
weath=weath.dropna(subset=['TMAX','GHI','AWND','TMIN'])

##make sure all units correspond. Use metric system units, including C
weath.loc[:,'AWND']=weath.loc[:,'AWND']*.447 ##data is in mi/hr instead of m/s - conversion


length=round((.8*len(weath)))
train=weath.iloc[1:length,:]
end=len(weath)
test=weath.iloc[length:end,:]


trainFeat=train.loc[:,['TMAX','Month','AWND','CloudCov']]
trainState=np.ravel(train.loc[:,['rain1']])
testFeat=test.loc[:,['TMAX','Month','AWND','CloudCov']]
testState=np.ravel(test.loc[:,['rain1']])

logreg_precip=LogisticRegression(max_iter=10000,random_state=0,multi_class='multinomial',solver='newton-cg')
logreg_precip.fit(trainFeat,trainState)

##predict test data and compare 
predict_precip=logreg_precip.predict(testFeat)
metrics.confusion_matrix(testState,predict_precip)
print("Accuracy:",metrics.accuracy_score(testState,predict_precip))

##once wet or dry state is determined, randomly sample from empirically 
##derived discrete distribution to decide wet or very wet 
##probability = #wet or very wet days/(#wet days + #very wet days) for every given month
allFeat=weath.loc[:,['TMAX','Month','AWND','CloudCov']]
predict_precip_full=logreg_precip.predict(allFeat)
predict_precip_full=pd.DataFrame(predict_precip_full)
simRain=pd.concat([weath.reset_index(),predict_precip_full],axis=1)

allState=weath.loc[:,['rain1']]
print("Accuracy:",metrics.accuracy_score(allState,predict_precip_full))

simRain.rename(columns={simRain.columns[49]:'simRainType1'},inplace=True)

simRain['simRainType']=np.zeros(shape=(len(simRain),1))
simRain.iloc[0,50]=1
for i in range(1,len(simRain)): 
    if simRain.iloc[i,49]==0:
        simRain.iloc[i,50]=0
    else:
        if simRain.iloc[(i-1),50]==0: ##when the day before is dry 
            x=np.random.uniform(0,1,1)
            if x<=simRain.iloc[i,25]:
                simRain.iloc[i,50]=1
            else: 
                simRain.iloc[i,50]=2
        elif simRain.iloc[(i-1),50]==1: ##when the day before is wet 
            x=np.random.uniform(0,1,1)
            if x<=simRain.iloc[i,21]:
                simRain.iloc[i,50]=1
            else: 
                simRain.iloc[i,50]=2
        else:                           ##when the day before is very wet
            x=np.random.uniform(0,1,1)
            if x<=simRain.iloc[i,24]:
                simRain.iloc[i,50]=1
            else:
                simRain.iloc[i,50]=2
    
##error plot -- looks much better on rain1 than on rain 
simRainDays1=simRain.groupby(['Month']).agg({'simRainType1':{'avg':'mean','sd':'std'}})
histRainDays1=weath.groupby(['Month']).agg({'rain1':{'avg':'mean','sd':'std'}})

plt.errorbar(np.arange(12), simRainDays1.iloc[:,0], simRainDays1.iloc[:,1], fmt='bv',lw=3)
plt.errorbar(np.arange(12), histRainDays1.iloc[:,0], histRainDays1.iloc[:,1], fmt='ro',lw=3,alpha=.5)
    
simRainDays2=simRain.groupby(['Month']).agg({'simRainType':{'avg':'mean','sd':'std'}})
histRainDays2=weath.groupby(['Month']).agg({'rain':{'avg':'mean','sd':'std'}})

plt.errorbar(np.arange(12), simRainDays2.iloc[:,0], simRainDays2.iloc[:,1], fmt='bv',lw=3)
plt.errorbar(np.arange(12), histRainDays2.iloc[:,0], histRainDays2.iloc[:,1], fmt='ro',lw=3,alpha=.5)
      
sns.kdeplot(simRain.loc[:,'simRainType'])
sns.kdeplot(weath.loc[:,'rain'])

#simRain.to_csv('C:\\Users\\rcuppari\\Desktop\\validRain.csv')

count2S=simRain[simRain['simRainType']==2]
count2A=weath[weath['rain']==2]
count1S=simRain[simRain['simRainType']==1]
count1A=weath[weath['rain']==1]
count0S=simRain[simRain['simRainType']==0]
count0A=weath[weath['rain']==0]



##now do this for the synthetic data 
syn_weather=pd.read_csv('C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\synthetic_maxmin_data_syn2.csv')
syn_weather=syn_weather.iloc[:,1:]
synFeat=syn_weather.loc[:,['synMax','month','syn_wind','syn_CC']]
for i in range(0,len(synFeat)): 
    for j in range (0,len(synFeat.columns)):
        if math.isnan(synFeat.iloc[i,j])==True: 
            synFeat.iloc[i,j]=synFeat.iloc[(i-1),j]
    
syn_pstate=pd.DataFrame(logreg_precip.predict(synFeat))
syn_weather=pd.concat([syn_weather.reset_index(),syn_pstate],axis=1)
syn_weather.rename(columns={'0':'synRainType1'},inplace=True)
syn_weather['simRainType']=np.zeros(shape=(len(syn_weather),1))
weath=weath.iloc[:,1:]
prob_intensity=weath.iloc[0:365,[15,16,17,18,19,20,21,22]]

syn_weather=syn_weather.merge(prob_intensity,how='left',on='daynum')
syn_weather=syn_weather.iloc[:,1:]
syn_weather.iloc[0,11]=syn_weather.iloc[0,10]
for i in range(1,len(syn_weather)): 
    if syn_weather.iloc[i,10]==0:
        syn_weather.iloc[i,11]=0
    elif syn_weather.iloc[i,10]==1 and syn_weather.iloc[(i-1),11]==1: ##wet to rainy transitions
        x=np.random.uniform(0,1,1)
        if x <= syn_weather.iloc[i,12]:
            syn_weather.iloc[i,11]=1
        else: 
            syn_weather.iloc[i,11]=2
            
    elif syn_weather.iloc[i,10]==1 and syn_weather.iloc[(i-1),11]==0: ##dry to rainy transitions
        x=np.random.uniform(0,1,1)
        if x <= syn_weather.iloc[i,16]:
            syn_weather.iloc[i,11]=1
        else: 
            syn_weather.iloc[i,11]=2
    else: ##very wet to rainy transitions
        x=np.random.uniform(0,1,1)
        if x <= syn_weather.iloc[i,15]:
            syn_weather.iloc[i,11]=1
        else: 
            syn_weather.iloc[i,11]=2


sns.kdeplot(simRain.iloc[:,50])
sns.kdeplot(weath.loc[:,'rain'])

simRainDays=syn_weather.groupby(['month']).agg({'simRainType':{'avg':'mean','sd':'std'}})
histRainDays= weath.groupby(['Month']).agg({'rain':{'avg':'mean','sd':'std'}})

plt.errorbar(np.arange(12), simRainDays.iloc[:,0], simRainDays.iloc[:,1], fmt='bv',lw=3)
plt.errorbar(np.arange(12), histRainDays.iloc[:,0], histRainDays.iloc[:,1], fmt='ro',lw=3)


syn_weather.to_csv('C:\\Users\\rcuppari\\Desktop\\syn_weather150yr_pstates_ownTrendFinal.csv')
##use a truncated gamma distribution to generate random samples of precipitation
##truncate by dividing the density by the probability of x within that range (reduces to 1) 

##start by fitting gamma distribution by month 
Jan=weath[weath['Month']==1]
Feb=weath[weath['Month']==2]
Mar=weath[weath['Month']==3]
Apr=weath[weath['Month']==4]
May=weath[weath['Month']==5]
Jun=weath[weath['Month']==6]
Jul=weath[weath['Month']==7]
Aug=weath[weath['Month']==8]
Sep=weath[weath['Month']==9]
Oct=weath[weath['Month']==10]
Nov=weath[weath['Month']==11]
Dec=weath[weath['Month']==12]

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
































