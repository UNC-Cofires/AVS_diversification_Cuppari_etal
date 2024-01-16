# -*- coding: utf-8 -*-
"""
Created on Fri Apr 19 17:24:26 2019

@author: YSu
"""

import pandas as pd
import numpy as np
import scipy.stats as st
import matplotlib.pyplot as plt
from statsmodels.distributions.empirical_distribution import ECDF

os.chdir('C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis')
solar=pd.read_csv('Solar_data_GHI_input.csv',header=0,index_col=0)
y=18

result_1=pd.DataFrame()
ave=pd.DataFrame()
std=pd.DataFrame()
clear_sky=pd.DataFrame()
daily=pd.DataFrame()
for j in range(1,10):
    site_name='Site'+str(j)
    ck_name=site_name+' clearsky'
    for i in range(1,13):
        name='moonth'+ str(i)
        locals()[name]=solar.loc[solar['Month']==i]
        
        name2='site1_' + str(i)
        locals()[name2]=locals()[name][site_name].values
        
        name22='site1_cs_' + str(i)
        locals()[name22]=locals()[name][ck_name].values
    #month1=solar.loc[solar['Month']==1]
    #site1=month1['Site1'].values
    #
    #site1=np.reshape(site1,(31*20,24))
    for i in range(1,13):
        if i in[1,3,5,7,8,10,12]:
            n=31
        elif i in [4,6,9,11]:
            n=30
        else:
            n=28
            
        name='site1_' + str(i)
        locals()[name]=np.reshape(locals()[name],(n*y,24))
        
        name0='site1_cs_' + str(i)
        locals()[name0]=np.reshape(locals()[name0],(n*y,24))
        
        name2='daily_site1_' + str(i)
        locals()[name2]=np.sum(locals()[name],axis=1)
        
        name21='daily_site1_year' + str(i)
        locals()[name21]=np.reshape(locals()[name2],(18,n))
        
        name22='daily_site1_cs_' + str(i)
        locals()[name22]=np.sum(locals()[name0],axis=1)
    
        name3='clearsky_site1_' + str(i)
        locals()[name3]=np.reshape(locals()[name22],(y,n))
        locals()[name3]=np.max(locals()[name3],axis=0)
        
        name4='cloudcoverage_site1_' + str(i)
        locals()[name4]=locals()[name3] - np.reshape(locals()[name2],(y,n))
        
        name5='cloudcoverage_long_site1_' + str(i)
        locals()[name5]=np.reshape(locals()[name4],n*y)
        
        name6='normalized_site1_' + str(i)
        locals()[name6]=(locals()[name5]-np.mean(locals()[name5]))/np.std(locals()[name5])
    
    site1_max=[]    
    
    cloud_max=[]
    daily_long=[]
    for i in range(1,13):
        name='clearsky_site1_' + str(i)
        site1_max=np.concatenate((site1_max,locals()[name]))
    
    for x in range(0,18):
        for i in range(1,13):
            name2='cloudcoverage_site1_' + str(i)
            cloud_max=np.concatenate((cloud_max,locals()[name2][x,:]))
            
            name3='daily_site1_year' +str(i)
            daily_long=np.concatenate((daily_long,locals()[name3][x,:]))
        
    long_cloud=np.reshape(cloud_max,(365,y))
    
    m_long=np.mean(long_cloud,axis=1)
    std_long=np.std(long_cloud,axis=1)
    
    ave[site_name]=m_long
    std[site_name]=std_long
    clear_sky[site_name]=site1_max
    daily[site_name]=daily_long
    
    
    
    normalized_cloud=(long_cloud-np.reshape(m_long,(365,1)))/np.reshape(std_long,(365,1))
    normalized_cloud=np.reshape(normalized_cloud,365*y)
    
    result_1[site_name]=normalized_cloud

result_1.to_csv('res_irr2.csv')
ave.to_csv('ave_irr2.csv')
std.to_csv('std_irr2.csv')
clear_sky.to_csv('clear_sky2.csv')
daily.to_csv('daily_irr2.csv')
#    smoothed = np.zeros((365,1))
#    lag =30
#    max_extended = np.concatenate((site1_max[-lag:],site1_max,site1_max[0:lag]))
#    
#    for i in range(0,365):
#        smoothed[i] = np.mean(max_extended[i:i+lag*2])
#    
#    
#    #smoothed=smoothed*1.03
#    #smoothed[22:100]=smoothed[22:100]
#    #smoothed[31:90]=smoothed[31:90]*1.1
#    #smoothed[40:100]=smoothed[40:100]+190
#    
#    plt.plot(site1_max)
#    
#    plt.plot(smoothed)
#    
    
#    a=np.log(cloudcoverage_long_site1_1+2000)
#    a=np.log(cloud_max+1000)
#    
#    rank=st.rankdata(a)
#    p=rank/(len(rank)+1)
#    new=st.norm.ppf(p)
#    
#    b=ECDF(a)
#    n2=b(a)
#    n3=st.norm.ppf(n2)
#    n3[n3>20]=2
    
    

#daily_site1=np.sum(site1,axis=1)
#
#clear_sky=np.reshape(daily_site1,(20,31))
#
#clear_sky=np.max(clear_sky,axis=0)
#
#cloud_coverage= clear_sky-np.reshape(daily_site1,(20,31))
#
#cloud_coverage_long=np.reshape(cloud_coverage,620)
#
#w=(cloud_coverage_long-np.mean(cloud_coverage_long))/np.std(cloud_coverage_long)
#
#a,c,l,s = st.exponweib.fit(w)
#n=st.exponweib.rvs(a,c,l,s,600)
#
#
#def running_mean(x, N):
#    cumsum = np.cumsum(np.insert(x, 0, 0)) 
#    return (cumsum[N:] - cumsum[:-N]) / float(N)
#
#
#plt.hist(n)
#
#st.exponweib.pdf(w, *st.exponweib.fit(w, 1, 1, scale=2, loc=0))
#plt.plot(w,st.exponweib.pdf(w, *st.exponweib.fit(w)),'r.')
#plt.hist(w,density=True)
#
#c=running_mean(clear_sky,5)
#
#plt.plot(c)
#plt.plot(clear_sky)