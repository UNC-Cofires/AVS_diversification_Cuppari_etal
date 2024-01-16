rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(pracma)

syn_weather=read.csv("D:\\Research\\Synthetic_results\\Synthetic_weather\\syn_weather200yr_rain.csv")
syn_weather=syn_weather[,-1]

##Elnaz's model run as frequently as possible##
## E=function(R,temp,ea,wind)##
##for it to work, need to use mean and not summed irradiation
weather=na.omit(syn_weather)
length=nrow(syn_weather)
index=1:length
A=.005 
refEff=.135
sigma=(5.67*10^-8) ##Boltzman constant - kg s???1 K???4 ##
lengthPan=1.5 #m, assumed#
kAir=.026 #W/MK, thermal conductivity of air#
Pr=.707 ## no units, Prandtl number of dry air##
nu=1.57*10^-5 #m^2s^-1 kinematic viscosity of air#
refTemp=273+25 ##Kelvins##
powgen=matrix(nrow=length,ncol=16,0)
acres=20 ##total size of solar farm plot
coverage=.7 ##amount of land actually covered by panels
##based on NREL Land Use Requirement Report, Tables 3 & 4
land=acres*coverage ##land usage in acres 
dc_ac=.95 ##conversion efficiency 
albedo=.2 
conv=4046.86 ##m^2/acre
EffGHI=matrix(c(rep(0,length),1))
EffGHI2=matrix(c(rep(0,length),1))

##For AVS though, even less land available: from .8 of the original amount
sol_reduction=.8
landAVS=land*sol_reduction

colnames(powgen)=c("Year","Month","Day","GHI","Wind","Temp",
                   "rain","prcp","GenGHI","UnitGen","GHIEff","rev","AVSgen",
                   "unitAVSgen","AVSeff","AVSrev")

for (i in index) {
  wind=weather[i,7]
  temp=weather[i,6]+273 ##TOBS - ground and air temperature are assumed to be the same. Measured in C##
  dewpoint=weather[i,5]+273
  ea=.611^((17.27*dewpoint)/(237.7+dewpoint)) ##measured vapor pressure of water 
  
  GHI=weather[i,8]/24 ##watts/m2 HOURLY (the model is designed to run hourly only)
  h=.036*(kAir/lengthPan)*((wind*lengthPan/nu)^(4/5))*Pr^(1/3)
  
  efunGHI=function(T_p) { ##T_p is panelTemp##
    
    (1-albedo-refEff*(1-A*(T_p-refTemp)))*GHI+sigma*(temp^4)*1.24*(ea/temp)^(1/7)+sigma*(temp^4)-2*h*(T_p-temp)-2*sigma*T_p^4
  }
  
  ##using GHI 
  panelTempGHI=fzero(efunGHI,300)
  panelTempGHI=as.numeric(panelTempGHI[1])
  EffGHI[i,1]=refEff*(1-A*(panelTempGHI[1]-refTemp)) ##actual efficiency of panel##
  
  ##powgen
  powgen[i,9]=EffGHI[i,1]*GHI*conv*land*24/1000000 ##watts/m2-hr * acre/m2*acres*1 MW/1000000 w
  powgen[i,10]=EffGHI[i,1]*GHI*conv*24/1000000 ##just production per acre
  powgen[i,11]=EffGHI[i,1]
  
  
  powgen[i,1]=weather[i,9] #Year
  powgen[i,2]=weather[i,11] #Month
  powgen[i,3]=weather[i,10] #Day
  powgen[i,4]=weather[i,8] ##GHI
  powgen[i,5]= weather[i,7] #wind
  powgen[i,6]=weather[i,6] #temp
  powgen[i,7]=weather[i,13] #rain
  powgen[i,8]=weather[i,16] ##prcp
  
  
  if (powgen[i,9]<0){
    powgen[i,9]=0
  }
  
  if (powgen[i,10]<0){
    powgen[i,10]=0
  }
  if (powgen[i,11]<0){
    powgen[i,11]=0
  }
}

powgen=as.data.frame(powgen)
plot(powgen$GHIEff)

eprices=read.csv("D:\\Research\\PNW_prices_200_FIX.csv")

##prices are $/MWh
powgen=powgen[1:73000,]
powgen2=cbind(powgen,eprices)
powgen2=powgen2[,-17]

for (i in 2:nrow(powgen2)) {
  if (is.na(powgen2[i,17])==TRUE) {
    powgen2[i,17]=powgen2[(i-1),17]
  }
}
powgen2=na.omit(powgen2)

powgen2[,12]=powgen2[,9]*powgen2[,17] ##total revenue of non-AVS solar farm 
powgen2[,16]=powgen2[,13]*powgen2[,17] ##total revenue of AVS farm 

powgen2$revM=powgen2[,10]*powgen2[,17] ##per acre revenue of non-AVS farm
powgen2$AVSrevM=powgen2[,14]*powgen2[,17]##per acre revenue of AVS farm

yr_rev=dplyr::group_by(powgen2,Year) %>%
  summarize(gen=sum(GenGHI),AVSgen=sum(AVSgen),
            UnitGen=sum(UnitGen),unitAVSgen=sum(unitAVSgen),
            rev=sum(rev),AVSrev=sum(AVSrev),
            revM=sum(revM),AVSrevM=sum(AVSrevM),
            temp=sum(Temp),prcp=sum(prcp),
            wind=sum(Wind),ghi=mean(GHI),rain=sum(rain))

mon_rev=dplyr::group_by(powgen2,Year,Month) %>%
  summarize(gen=sum(GenGHI),AVSgen=sum(AVSgen),
            UnitGen=sum(UnitGen),unitAVSgen=sum(unitAVSgen),
            rev=sum(rev),AVSrev=sum(AVSrev),
            unitRev=sum(revM),unitAVSrev=sum(AVSrevM),
            temp=sum(Temp),prcp=sum(prcp),
            wind=sum(Wind),ghi=mean(GHI),rain=sum(rain))

mon_rev$date=zoo::as.yearmon(paste(mon_rev$Year, mon_rev$Month), "%Y %m")

ggplot(mon_rev)+
  geom_density(aes(AVSgen,color="AVS"))+
  geom_density(aes(gen,color="non-AVS"),alpha=.5)+
  ggtitle("Monthly Generation on Solar Plot (AVS v Non-AVS)")+
  ylab("Revenue ($)")

ggplot(mon_rev)+
  geom_density(aes(unitAVSgen,fill="AVS"))+
  geom_density(aes(UnitGen,fill="non-AVS"),alpha=.5)+
  ggtitle("Monthly Normalized Generation (AVS v Non-AVS)")+
  ylab("Revenue ($/acre)") ##note that they are identical - microclim d/n impact

ggplot(yr_rev)+
  geom_density(aes(AVSrev,fill="AVS"))+
  geom_density(aes(rev,fill="non-AVS"),alpha=.5)+
  ggtitle("Revenue Distribution on Solar Plot (AVS v Non-AVS)")+
  xlab("Annual Revenue ($)")

ggplot(yr_rev)+
  geom_density(aes(AVSrevM,fill="AVS"))+
  geom_density(aes(revM,fill="non-AVS"),alpha=.5)+
  ggtitle("Normalized Revenue Distribution (AVS v Non-AVS)")+
  xlab("Annual Revenue ($/acre)")

ggplot(yr_rev)+
  geom_line(aes(x=Year,y=AVSrev,color="AVS"),size=1)+
  geom_line(aes(x=Year,y=rev,color="non-AVS"),size=1)+
  ggtitle("Annual Revenue (AVS v Non-AVS)")+
  ylab("Revenue ($)")

ggplot(yr_rev)+
  geom_line(aes(x=Year,y=AVSrevM,color="AVS"),size=2)+
  geom_line(aes(x=Year,y=revM,color="non-AVS"),size=1)+
  ggtitle("Normalized Annual Revenue (AVS v Non-AVS)")+
  ylab("Revenue ($/acre)")

var(yr_rev$rev)
var(yr_rev$AVSrev)

mean(yr_rev$rev)
mean(yr_rev$AVSrev)

var(yr_rev$revM)
var(yr_rev$AVSrevM)

mean(yr_rev$revM)
mean(yr_rev$AVSrevM)

library(corrplot)
corrplot(cor(yr_rev,use="complete.obs"),method="color")
