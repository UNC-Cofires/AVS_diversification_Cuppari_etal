# -*- coding: utf-8 -*-
"""
Created on Fri Sep 13 14:22:52 2019

@author: rcuppari
"""

acres=20 ##total size of solar farm plot
weather=pd.read_csv("C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Synthetic_results\\Synthetic_weather\\syn_weather150yr_rain_ownTrend2.csv")
##inputs required: wind (m/s), tavg (C), tmax (C), tmin (C), irradiation (w/m2)

def powergen(acres,weather):
##############################################################################
##############################################################################
##script intended to generate power production estimates for a specific site
##based on OSU thermodynamic model 
##############################################################################
##############################################################################
    A=.005 
    refEff=.135
    sigma=(5.67*10^-8) ##Boltzman constant 
    lengthPan=1.5 #m, assumed#
    kAir=.026 #W/MK, thermal conductivity of air#
    Pr=.707 ## no units, Prandtl number of dry air##
    nu=1.57*10^-5 #m^2s^-1 kinematic viscosity of air#
    refTemp=273+25 ##Kelvins##
    coverage=.7 ##amount of land actually covered by panels
    ##based on NREL Land Use Requirement Report, Tables 3 & 4
    land=acres*coverage ##land usage in acres 
    dc_ac=.96 ##conversion efficiency 
    albedo=.2 
    conv=4046.86 ##m^2/acre

    ##For AVS less land available: .8 of the original amount
    sol_reduction=.8
    landAVS=land*sol_reduction

    ##create array 
    powgen=[]
    EffGHI=[]
    length=len(weather)
        
    for i in range(0,len(length)):
        wind=weather.loc[i,'syn_wind']
        temp=weather.loc[i,'syn_temp']+273 ##Tavg in Kelvins
        tmax=weather.loc[i,'synMax']
        tmin=weather.loc[i,'synMin']

        etmax=.6108*exp((17.27*tmax)/(tmax+237.3)) ##saturated vapor pressure##
        etmin=.6108*exp((17.27*tmin)/(tmin+237.3)) ##actual vapor pressure##

        ea=etmin
        es=(etmax+etmin)/2
  
        GHI=weather.loc[i,'syn_ghi']/24 ##watts/m2 HOURLY (the model is designed to run hourly only)
        h=.036*(kAir/lengthPan)*((wind*lengthPan/nu)^(4/5))*Pr^(1/3)
  ##FIX: WHAT'S A FUNCTION? 
        efunGHI=fun(T_p) { ##T_p is panelTemp##
    
        (1-albedo-refEff*(1-A*(T_p-refTemp)))*GHI+sigma*(temp^4)*1.24*(ea/temp)^(1/7)+sigma*(temp^4)-2*h*(T_p-temp)-2*sigma*T_p^4
  
  ##using GHI 
  panelTempGHI=fzero(efunGHI,300)
  panelTempGHI=as.numeric(panelTempGHI[0])
  EffGHI.iloc[i,0]=refEff*(1-A*(panelTempGHI[0]-refTemp)) ##actual efficiency of panel##
  
  ##powgen
  powgen[i,7]=EffGHI[i,1]*GHI*conv*land*12/1000000 ##watts/m2-hr * m2/acre*acres*1 MW/1000000 w
  powgen[i,8]=EffGHI[i,1]*GHI*conv*12/1000000 ##just production per acre
  powgen[i,11]=EffGHI[i,1]*GHI*landAVS*conv*12/1000000 ##production on AVS 
  powgen[i,12]=EffGHI[i,1]*GHI*conv*sol_reduction*12/1000000 ##unit AVS Gen
  powgen[i,9]=EffGHI[i,1]
  
  powgen[i,0]=weather[i,10] #Year
  powgen[i,1]=weather[i,12] #Month
  powgen[i,2]=weather[i,11] #Day
  powgen[i,3]=weather[i,8] ##GHI
  powgen[i,4]=weather[i,7] #wind
  powgen[i,5]=weather[i,6] #temp
  powgen[i,6]=weather[i,21] #prcp
  powgen[i,15]=weather[i,22]
  
  
  if (powgen.iloc[i,7]<0){
    powgen.iloc[i,7]=0
  }
  
  if (powgen.iloc[i,8]<0){
    powgen.iloc[i,8]=0
  }
  if (powgen.iloc[i,9]<0){
    powgen.iloc[i,9]=0
  }
}

powgen.columns=["Year","Month","Day","GHI","Wind","Temp",
                   "prcp","GenGHI","UnitGen","GHIEff","AVSgen",
                   "unitAVSgen","AVSeff"]
    return None  
