# -*- coding: utf-8 -*-
"""
Created on Wed Jul 17 12:02:04 2019

@author: rcuppari
"""
import pandas as pd
import numpy as np
import os
from sklearn import metrics
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.neighbors import KNeighborsRegressor
import scipy.stats as st
import sklearn.neighbors
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LogisticRegression

os.chdir('C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master')
##import detrended weather data
detWeath=pd.read_csv('C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Salem_weather_fulldetMon.csv')
##drop any rows with missing data 
detWeath=detWeath.dropna(subset=['TMIN.1','TMAX.1','TAVG.1','AWND.1','CC.1'])

##use 80% of data to train and 20% to test
end=len(detWeath)
#split=round(int(.2*end))
split=7302-730
train=detWeath.iloc[730:split,:]
test=detWeath[(detWeath['X'] <= detWeath.iloc[0,0]+729) | (detWeath['X'] > detWeath.iloc[0,0]+split+1)]

sns.kdeplot(train.loc[:,'CC.1'])
sns.kdeplot(test.loc[:,'CC.1'])


sns.kdeplot(detWeath.loc[:,'TMAX.1'])
sns.kdeplot(train.loc[:,'TMAX.1'])
sns.kdeplot(test.loc[:,'TMAX.1'])

##separate out dependent variables (tmax and tmin) and independent variables 
tmin_train=train.loc[:,['TMIN.1']]
tmin_test=test.loc[:,['TMIN.1']]

tmax_train=train.loc[:,['TMAX.1']]
tmax_test=test.loc[:,['TMAX.1']]

train_input=train.loc[:,['AWND.1','CC.1','TAVG.1']]
test_input=test.loc[:,['AWND.1','CC.1','TAVG.1']]

##train the model
knn=KNeighborsRegressor(n_neighbors=10,weights='distance',algorithm='auto')

##predict tmax residuals
knnMax=knn.fit(train_input,tmax_train)
predMaxRes=knnMax.predict(test_input)
predMaxRes=pd.DataFrame(predMaxRes)

##predict tmin residuals 
knnMin=knn.fit(train_input,tmin_train)
predMinRes=knnMin.predict(test_input)
predMinRes=pd.DataFrame(predMinRes)

metrics.r2_score(predMaxRes,tmax_test)
metrics.r2_score(predMinRes,tmin_test)

##re-season detrended data to find actual temperature values
predMax=pd.concat([predMaxRes,test.reset_index()],axis=1)
predMin=pd.concat([predMinRes,test.reset_index()],axis=1)

predMax.loc[:,'synValMax']=np.zeros(len(predMax))
predMin.loc[:,'synValMin']=np.zeros(len(predMin))

for i in range(0,len(predMax)):
    predMax.iloc[i,50]=predMax.iloc[i,0]*predMax.iloc[i,34]+predMax.iloc[i,33]

for i in range(0,len(predMin)):
    predMin.iloc[i,50]=predMin.iloc[i,0]*predMin.iloc[i,36]+predMin.iloc[i,35]

maxTestVals=predMax.loc[:,'TMAX']
minTestVals=predMin.loc[:,'TMIN']   
synMax=predMax.loc[:,'synValMax']
synMin=predMin.loc[:,'synValMin']

round(metrics.r2_score(synMax,maxTestVals),4)
round(metrics.r2_score(synMin,minTestVals),4)

##now that the model has been validated simulate across entire historical timeseries
hist_weath=detWeath.loc[:,['AWND.1','CC.1','TAVG.1']]

predMaxAll=knnMax.predict(hist_weath)
predMinAll=knnMin.predict(hist_weath)

predMaxAll=pd.DataFrame(predMaxAll)
predMinAll=pd.DataFrame(predMinAll)

detWeath['simMax']=np.zeros(shape=(len(detWeath),1))
detWeath['simMin']=np.zeros(shape=(len(detWeath),1))

for i in range(0,len(detWeath)):
    detWeath.iloc[i,48]=predMaxAll.iloc[i,0]*detWeath.iloc[i,32]+detWeath.iloc[i,31]
    detWeath.iloc[i,49]=predMinAll.iloc[i,0]*detWeath.iloc[i,34]+detWeath.iloc[i,33]

synMaxTot=detWeath.loc[:,'simMax']
actMax=detWeath.loc[:,'TMAX']
synMinTot=detWeath.loc[:,'simMin']
actMin=detWeath.loc[:,'TMIN']

plt.plot(synMaxTot)
plt.plot(actMax)

round(metrics.r2_score(detWeath.iloc[:,43],predMaxAll.iloc[:,0]),4) ##not sure why this is happening?
round(metrics.r2_score(detWeath.iloc[:,44],predMinAll.iloc[:,0]),4)

round(metrics.r2_score(synMaxTot,actMax),4) ##the residuals are off somewhere/somehow
round(metrics.r2_score(synMinTot,actMin),4)

meanMaxSim=detWeath.groupby(['Month']).agg({'simMax':'mean','TMAX':'mean'})
sdMaxSim=   detWeath.groupby(['Month']).agg({'simMax':'std','TMAX':'std'}) ##so I guess this is okay?
plt.errorbar(np.arange(12), meanMaxSim.loc[:,'simMax'], sdMaxSim.loc[:,'simMax'], fmt='bv',lw=3)
plt.errorbar(np.arange(12), meanMaxSim.loc[:,'TMAX'], sdMaxSim.loc[:,'TMAX'], fmt='ro',lw=3,alpha=.5)

plt.scatter(synMaxTot,actMax,marker='o')
plt.scatter(synMax,maxTestVals)

#detWeath.to_csv('C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\synthetic_maxmin_data_hist.csv')

##now do for entire simulated dataset
##irradiation and wind are directly simulated so need to back out residuals
##have stdev and mean for each day, simply extract first 365 values from Wind_Temp_Std 
##and other files. Also have directly simulated residuals for temperature 
avg_w_t=pd.read_csv('Historical_weather_analysis/WIND_TEMP_ave.csv')
avg_w_t=avg_w_t.loc[:,['SALEM_T','SALEM_W']]
avg_w_t=avg_w_t.iloc[0:365]

sd_w_t=pd.read_csv('Historical_weather_analysis/WIND_TEMP_std.csv')
sd_w_t=sd_w_t.loc[:,['SALEM_T','SALEM_W']]
sd_w_t=sd_w_t.iloc[0:365]

avg_ghi=pd.read_csv('Historical_weather_analysis/ave_irr.csv')
avg_ghi=avg_ghi.loc[:,'Site8']

sd_ghi=pd.read_csv('Historical_weather_analysis/std_irr.csv')
sd_ghi=sd_ghi.loc[:,'Site8']

syn_wind_temp=pd.read_csv('C:/Users/rcuppari/Desktop/synthetic_weather_data150.csv')
syn_wind_temp=syn_wind_temp.loc[:,['SALEM_T','SALEM_W']]
syn_wind_temp=syn_wind_temp.iloc[0:74095,:] ##200 years worth

syn_ghi=pd.read_csv('C:/Users/rcuppari/Desktop/synthetic_irradiance_data150.csv')
syn_ghi=syn_ghi.loc[:,'Site8'] ##site 8 = salem

cloudCov=pd.read_csv('C:/Users/rcuppari/OneDrive - University of North Carolina at Chapel Hill/Research/Salem_Weather_fullDet.csv')
cloudCov=cloudCov.dropna(subset=['CloudCov'])
cloudCov=cloudCov.iloc[0:365,:]
cloudCov=cloudCov.loc[:,['daynum','maxCS','mCC','sdCC']]

avg_w_t['daynum']=1
avg_ghi=pd.DataFrame(avg_ghi)
avg_ghi['daynum']=1
sd_w_t['daynum']=1
sd_ghi=pd.DataFrame(sd_ghi)
sd_ghi['daynum']=1

for i in range(1,365):
    avg_w_t.iloc[i,2]=avg_w_t.iloc[(i-1),2]+1
    avg_ghi.iloc[i,1]=avg_ghi.iloc[(i-1),1]+1
    sd_w_t.iloc[i,2]=sd_w_t.iloc[(i-1),2]+1
    sd_ghi.iloc[i,1]=sd_ghi.iloc[(i-1),1]+1

syn_wind_temp['daynum']=1
syn_ghi=pd.DataFrame(syn_ghi)
syn_ghi['daynum']=1

##adding daynum 
for i in range(1,len(syn_ghi)):
    if (syn_wind_temp.iloc[(i-1),2]<365):
        syn_wind_temp.iloc[i,2]=syn_wind_temp.iloc[(i-1),2]+1
        syn_ghi.iloc[i,1]=syn_ghi.iloc[(i-1),1]+1
    else: 
        syn_wind_temp.iloc[i,2]=1
        syn_ghi.iloc[i,1]=1
       
syn_wind_temp.columns=['syn_temp','syn_wind','daynum']
avg_w_t.columns=['avg_temp','avg_wind','daynum']
sd_w_t.columns=['sd_temp','sd_wind','daynum']
syn_ghi.columns=['syn_ghi','daynum']
avg_ghi.columns=['avg_ghi','daynum']
sd_ghi.columns=['sd_ghi','daynum']

syn_w_t_tot=syn_wind_temp.merge(avg_w_t, how='left', on="daynum")
syn_w_t_tot=syn_w_t_tot.merge(sd_w_t,how='left',on='daynum')
syn_ghi_tot=syn_ghi.merge(avg_ghi,how='left',on='daynum')
syn_ghi_tot=syn_ghi_tot.merge(sd_ghi,how='left',on='daynum')
syn_ghi_tot=syn_ghi_tot.merge(cloudCov,how='left',on='daynum')

syn_ghi_tot['syn_CC']=np.zeros(shape=(len(syn_ghi_tot),1))
syn_ghi_tot.loc[:,'syn_CC']=syn_ghi_tot.loc[:,'maxCS']-syn_ghi_tot.loc[:,'syn_ghi']

for i in range(0,len(syn_ghi_tot)):
    if syn_ghi_tot.iloc[i,7]<0: 
        syn_ghi_tot.iloc[i,7]=1 ##logical check: if cloud cover is negative (i.e. more irrad than historical max CS), make cloud cover = 1
        syn_ghi_tot.iloc[i,0]=syn_ghi_tot.iloc[i,4] ##and also make synthetic ghi equal to max CS
  
plt.plot(syn_ghi_tot.loc[:,'syn_CC'])
syn_ghi_tot=pd.DataFrame(syn_ghi_tot)

##detrend simulated inputs
sim_input =pd.DataFrame(np.zeros(shape=(len(syn_ghi_tot),5))) 
##need to add cloud cover
sim_input.columns=['syn_res_t','syn_res_w','syn_res_ghi','syn_res_CC','daynum']

for i in range(0,len(sim_input)):
    sim_input.iloc[i,0]=(syn_w_t_tot.iloc[i,0]-syn_w_t_tot.iloc[i,3])/syn_w_t_tot.iloc[i,5] ##temperature 
    sim_input.iloc[i,1]=(syn_w_t_tot.iloc[i,1]-syn_w_t_tot.iloc[i,4])/syn_w_t_tot.iloc[i,6] ##wind
    sim_input.iloc[i,2]=(syn_ghi_tot.iloc[i,0]-syn_ghi_tot.iloc[i,2])/syn_ghi_tot.iloc[i,3] ##GHI
    sim_input.iloc[i,3]=(syn_ghi_tot.iloc[i,7]-syn_ghi_tot.iloc[i,5])/syn_ghi_tot.iloc[i,6] ##cloud cover
    sim_input.iloc[i,4]=syn_ghi_tot.iloc[i,1] ##daynum

plt.plot(sim_input.iloc[:,3])

##predict using simulated average temperature (Celsius already), irradiation, and wind 
knn_input=sim_input.loc[:,['syn_res_w','syn_res_CC','syn_res_t']]

sim_max=pd.DataFrame(np.zeros(shape=(len(knn_input),3)))
sim_min=pd.DataFrame(np.zeros(shape=(len(knn_input),3)))

sim_max.columns=['daynum','synResMax','synMax']
sim_min.columns=['daynum','synResMin','synMin']

sim_max.iloc[:,0]=sim_input.loc[:,'daynum'] ##add daynum to
sim_min.iloc[:,0]=sim_input.loc[:,'daynum'] ##add daynum to

sim_max.iloc[:,1]=knnMax.predict(knn_input)
sim_min.iloc[:,1]=knnMin.predict(knn_input)

#plt.plot(sim_max.iloc[:,1])

tmax_stats=test.iloc[0:365,:]
tmax_stats=tmax_stats.loc[:,['daynum','mMax','sdMax']]
tmin_stats=test.iloc[0:365,:]
tmin_stats=tmin_stats.loc[:,['daynum','mMin','sdMin']]

sim_max=sim_max.merge(tmax_stats, how='left', on="daynum")
sim_min=sim_min.merge(tmin_stats, how='left', on="daynum")

##re-season 
for i in range(0,len(knn_input)): 
    sim_max.iloc[i,2]=((sim_max.iloc[i,1]*sim_max.iloc[i,4]+sim_max.iloc[i,3]))
    sim_min.iloc[i,2]=((sim_min.iloc[i,1]*sim_min.iloc[i,4]+sim_min.iloc[i,3]))
    
    if sim_max.iloc[i,2]<sim_min.iloc[i,2]:
        sim_max.iloc[i,2]=sim_min.iloc[i,2]+1 ##logical check to make sure max is greater than min 

#plt.plot(sim_max.iloc[:,2])
#plt.plot(sim_min.iloc[:,2])
##now need to add leap years/date for AquaCrop 
##assume a starting date of 2020 (for ease of calculation, but should verify) 
syn_weather=pd.concat([sim_max.loc[:,['daynum','synMax']],sim_min.loc[:,'synMin'],syn_w_t_tot.loc[:,['syn_temp','syn_wind']],syn_ghi_tot.loc[:,['syn_ghi','syn_CC']]],axis=1)
dates=pd.DataFrame(np.zeros(shape=(len(syn_weather),3)))
syn_weather=pd.concat([syn_weather,dates],axis=1)

syn_weather.iloc[0,7]=2020 #year
syn_weather.iloc[0,8]=1 #day
syn_weather.iloc[0,9]=1 #month

##months with 30 days: Apr,Jun,Sep,Nov
##months with 31: J,Mar,May,Jul,Aug,Oct,Dec
##Feb=28

for i in range (1,len(syn_weather)):
    if syn_weather.iloc[i,0]<365:
        syn_weather.iloc[i,7]=syn_weather.iloc[(i-1),7]       
    else:
        syn_weather.iloc[i,7]=syn_weather.iloc[(i-1),7]+1
    
    if syn_weather.iloc[i,0] <32:
        syn_weather.iloc[i,8]=syn_weather.iloc[i,0]
        syn_weather.iloc[i,9]=1
    elif syn_weather.iloc[i,0] < 60: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-31),0]
        syn_weather.iloc[i,9]=2
    elif syn_weather.iloc[i,0] < 91:
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-59),0]
        syn_weather.iloc[i,9]=3
    elif syn_weather.iloc[i,0]< 121: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-90),0]
        syn_weather.iloc[i,9]=4
    elif syn_weather.iloc[i,0]< 152: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-120),0]
        syn_weather.iloc[i,9]=5
    elif syn_weather.iloc[i,0]< 182: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-151),0]
        syn_weather.iloc[i,9]=6
    elif syn_weather.iloc[i,0]< 213: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-181),0]
        syn_weather.iloc[i,9]=7
    elif syn_weather.iloc[i,0]< 244: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-212),0]
        syn_weather.iloc[i,9]=8
    elif syn_weather.iloc[i,0]< 274: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-243),0]
        syn_weather.iloc[i,9]=9
    elif syn_weather.iloc[i,0]< 305: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-273),0]
        syn_weather.iloc[i,9]=10
    elif syn_weather.iloc[i,0]< 335: 
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-304),0]
        syn_weather.iloc[i,9]=11
    else:
        syn_weather.iloc[i,8]=syn_weather.iloc[(i-335),0]
        syn_weather.iloc[i,9]=12
    
##join date to the syn_weather file
syn_weather.columns=['daynum','synMax','synMin','syn_temp','syn_wind','syn_ghi','syn_CC','year','day','month']
plt.plot(syn_weather.loc[:,'syn_CC'])


histMax=detWeath.groupby(['Month']).agg({'TMAX':{'mean','std'}})
simMax=syn_weather.groupby(['month']).agg({'synMax':{'mean','std'}})
plt.errorbar(np.arange(12), histMax.iloc[:,0], histMax.iloc[:,1], fmt='bv',lw=3)
plt.errorbar(np.arange(12), simMax.iloc[:,0], simMax.iloc[:,1], fmt='ro',lw=3,alpha=.5)
##also looks okay for the simulated stuff 


histCC=detWeath.groupby(['Month']).agg({'CloudCov':{'mean','std'}})
simCC=syn_weather.groupby(['month']).agg({'syn_CC':{'mean','std'}})
plt.errorbar(np.arange(12), histCC.iloc[:,0], histCC.iloc[:,1], fmt='bv',lw=3)
plt.errorbar(np.arange(12), simCC.iloc[:,0], simCC.iloc[:,1], fmt='ro',lw=3,alpha=.5)
##also looks okay for the simulated stuff 

histGHI=detWeath.groupby(['Month']).agg({'GHI':{'mean','std'}})
simGHI=syn_weather.groupby(['month']).agg({'syn_ghi':{'mean','std'}})
plt.errorbar(np.arange(12), histGHI.iloc[:,0], histGHI.iloc[:,1], fmt='bv',lw=3)
plt.errorbar(np.arange(12), simGHI.iloc[:,0], simGHI.iloc[:,1], fmt='ro',lw=3,alpha=.5)

syn_weather.to_csv('C:\\Users\\rcuppari\\Desktop\\syn_weather150_CC.csv')

#add leap years 
def is_leap(year):
    leap = False
    if year % 4 == 0:
        if year % 100 == 0:
            if year % 400 == 0:
                return True
            return False
        return True
    else:
        return False

    return leap

feb28=pd.DataFrame(np.zeros(shape=(len(syn_weather),1)))
syn_weather=pd.concat([syn_weather,feb28],axis=1)

for i in range(0,len(syn_weather)):
    if syn_weather.iloc[i,7]==28 and syn_weather.iloc[i,8]==2:
        syn_weather.iloc[i,9]=1
    else: 
        syn_weather.iloc[i,9]=0

count=0
for i in range(0,len(syn_weather)): 
    year=syn_weather.iloc[i,6]
    if is_leap(year)==True: ##NEED TO JUST TEST ON UNIQUE YEARS TO GET A COUNT
        count=count+1

for i in range(0,len(syn_weather)): 
    year=syn_weather.iloc[i,6]        
    if is_leap(year)==True:     
        for i in range(0,len(syn_weather)): 
            if syn_weather.iloc[i,9]==1: 
                leap_row=pd.DataFrame(np.zeros(shape=(count,9)))
                leap_row.iloc[0,0]=0
                leap_row.iloc[0,1]=0
                leap_row.iloc[0,2]=0
                leap_row.iloc[0,3]=0
                leap_row.iloc[0,4]=0
                leap_row.iloc[0,5]=0
                leap_row.iloc[0,6]=year
                leap_row.iloc[0,7]=29 ##day 
                leap_row.iloc[0,8]=2 ##month 
                ##add a row for each year that has a leap year
        
##arrange by year month day so that can take the feb 28 data and plug into feb 29 

###############################FIGURES########################################
plt.plot(sim_min.iloc[:,2])
plt.plot(sim_max.iloc[:,2])
plt.plot(detWeath.loc[:,'TMAX'])
plt.plot(detWeath.loc[:,'TMIN'])

plt.plot(syn_weather.iloc[0:1000,3])
plt.plot(detWeath.loc[:,'TAVG'])

plt.plot(syn_w_t_tot.loc[:,'syn_temp'])
plt.xlabel('day')
plt.ylabel('temperature (C)')
plt.title('Maximum, Minimum, and Average Simulated Temperatures')
plt.savefig('simTemps.png')
plt.legend()

tmin=(detWeath.loc[:,'TMIN']-32)*5/9
plt.hist([tmin,sim_min.iloc[:,2]],bins=100,density=True,
         color=['blue','red'],label=['simulated','observed'])
plt.xlabel('temperature (C)')
plt.ylabel('frequency')
plt.title('Side by Side histogram of Sim v Act Min Temp')
plt.legend()
#plt.savefig('sideBySideTMIN.png')

tmax=(detWeath.loc[:,'TMAX']-32)*5/9
plt.hist([tmax,sim_max.iloc[:,2]],bins=100,density=True,
         color=['blue','red'],label=['simulated','observed'])
plt.xlabel('temperature (C)')
plt.ylabel('frequency')
plt.title('Side by Side histogram of Sim v Act Max Temp')
plt.legend()
#plt.savefig('sideBySideTMIN.png')







