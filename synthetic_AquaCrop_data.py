# -*- coding: utf-8 -*-
"""
Created on Thu Aug 29 08:49:15 2019

@author: rcuppari
"""

############################################################
##this script is meant to calculate additional data required 
##to run AquaCrop, specifically relative humidity and 
##ensuring that every day of the year has data (leap year days are
##added as simply identical to the previous day)
##it also converts the file into the format required by AquaCrop
##the start date for the simulated data is ????? 

##calculate relative humidity 


##calculate ET0 (reference evapotranspiration) 


##output txt file that AquaCrop can read

Day=synthetic.iloc[:,4]
Month=synthetic.iloc[:,4]
Year=synthetic.iloc[:,4]
Tmin=synthetic.iloc[:,4]
Tmax=synthetic.iloc[:,4]
Prcp=synthetic.iloc[:,4]
ET0=synthetic.iloc[:,4]

##create file, writing to the AquaCrop folder
f=open("C:/Users/rcuppari/Desktop/CleanAqua/AquaCropOS_v50a/Input/synthetic_weather.txt","w+") 
f.write("%%%% ---------- Weather input time-series for AquaCropOS ---------- %%%%")
for i in range(0,len(synthetic_weather)):
    f.write(weatheroutArray[i,:]) ##not sure if this is exactly what I want
f.close()
    
    
weatheroutArray=[Day,Month,Year,Tmin,Tmax,Prcp,ET0]

filename=sprintf('C:/Users/rcuppari/Desktop/CleanAqua/AquaCropOS_v50a/Input/WeatherSalAct.txt');
fid=fopen(filename,'wt');
fprintf(fid,'%%%% ---------- Weather input time-series for AquaCropOS ---------- %%%%');
fprintf(fid,'\n');
fprintf(fid,'%%%% Day Month Year MinTemp MaxTemp Precipitation ReferenceET %%%%');
fprintf(fid,'\n');
for ii =1:size(weatheroutArray)
  fprintf(fid,'%g\t%g\t%g\t%f\t%f\t%f\t%f\t', weatheroutArray(ii,:));
  fprintf(fid,'\n');
end
fclose(fid);