##prcp plots for eugene
rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(Metrics)

actual=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\actualMult.csv")
predict=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\predictedStateMult.csv")
rainStateVal=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\rainStates_validationMult.csv")

colnames(actual)=c("day","val")
colnames(predict)=c("day","predict")
compare=full_join(actual,predict)

monthlyRain=group_by(rainStateVal,Year,Month) %>%
  summarize(sdR=sd(rain),dry=sum(dry),simState=sum(X0),
            sdP=sd(X0))

monthlyRain$month2=monthlyRain$Month+.5

yearlyRain=group_by(rainStateVal,Year)%>%
  summarize(sdR=sd(rain),rain=sum(rain),simState=sum(X0),sdP=sd(X0))

rrse(monthlyRain$simState,monthlyRain$rain)
rmse(monthlyRain$simState,monthlyRain$rain)

rrse(yearlyRain$simState,yearlyRain$rain)
rmse(yearlyRain$simState,yearlyRain$rain)

rsq=function(x,y) cor(x,y)^2
rsq(monthlyRain$simState,monthlyRain$rain)
rsq(yearlyRain$simState,yearlyRain$rain)
rsq(monthlyRain$sdR,monthlyRain$sdP)
rsq(yearlyRain$sdR,yearlyRain$sdP)

ggplot(monthlyRain)+
  geom_boxplot(aes(x=month2,y=rain,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=simState,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Number of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Wet Days, Monthly")+
  #annotate("text",x=7,y=28,label="r2=0.91")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("purple","orange"))#+

ggplot(monthlyRain)+
  geom_point(aes(x=rain,y=simState,color=factor(Month)))+
  geom_abline()+
  xlab("Observed Rainy Days")+
  ylab("Simulated of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Wet Days, Monthly")+
  facet_wrap(~Month)#+
  annotate("text",x=5,25,label="r2=0.91")
  
ggplot(monthlyRain)+
  geom_point(aes(x=sdR,y=sdP,color=factor(Month)))+
  geom_abline()+
  xlab("Observed Stdev")+
  ylab("Simulated Stdev")+
  ggtitle("Simulated vs. Observed Monthly Stdev in Wet Days")#+
  annotate("text",x=.2,.4,label="r2=0.87")
  
ggplot(yearlyRain)+
  geom_point(aes(x=rain,y=simState))+
  geom_abline()+
  ggtitle("Simulated v Observed Amount of Wet Days, Annually")
  #annotate("text",90,150,label="r2=0.89")+

####################WET AND DRY SPELLS###################
compare=compare[,-1]
lenDryA=0
spellsDryA=0
lenDrySpellA=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,1]==0) {
    if (compare[i,1]==compare[(i-1),1]) {
      lenDryA=lenDryA+1
    } else {
      lenDrySpellA[i]=lenDryA
      lenDryA=0
      spellsDryA=spellsDryA+1
    }
    avgLenDryAct=sum(lenDrySpellA,na.rm=TRUE)/spellsDryA
  }
}

lenDryT=0
spellsDryT=0
lenDrySpellT=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,2]==0) {
    if (compare[i,2]==compare[(i-1),2]) {
      lenDryT=lenDryT+1
    } else {
      lenDrySpellT[i]=lenDryT
      lenDryT=0
      spellsDryT=spellsDryT+1
    }
    avgLenDryTest=sum(lenDrySpellT,na.rm=TRUE)/spellsDryT
    
  }
}

##wet spells##
lenWetA=0
spellsWetA=0
lenWetSpellA=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,2]==1) {
    if (compare[i,1]==compare[(i-1),1]) {
      lenWetA=lenWetA+1
    } else {
      lenWetSpellA[i]=lenWetA
      lenWetA=0
      spellsWetA=spellsWetA+1
    }
    avgLenWetAct=sum(lenWetSpellA,na.rm=TRUE)/spellsWetA
  }
}

lenWetT=0
spellsWetT=0
lenWetSpellT=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,2]==1) {
    if (compare[i,2]==compare[(i-1),2]) {
      lenWetT=lenWetT+1
    } else {
      lenWetSpellT[i]=lenWetT
      lenWetT=0
      spellsWetT=spellsWetT+1
    }
    avgLenWetTest=sum(lenWetSpellT,na.rm=TRUE)/spellsWetT
  }
}


avgLenWetAct
avgLenWetTest
avgLenDryAct
avgLenDryTest

###########################
rainFull=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\simRain_full2.csv")

monthFull=group_by(rainFull,Year,Month) %>%
  summarize(rain=sum(PRCP),simP=sum(simP))

yearFull=group_by(rainFull,Year) %>%
  summarize(rain=sum(PRCP),simP=sum(simP))

rsq(monthFull$simP,monthFull$rain)
rsq(yearFull$simP,yearFull$rain)
rsq(rainFull$simP,rainFull$PRCP)

monthFull$month2=monthFull$Month+.5
ggplot(monthFull)+
  geom_boxplot(aes(x=month2,y=rain,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=simP,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Rainfall (mm)")+
  ggtitle("Simulated v Observed Amount of Rainfall, Monthly")+
  annotate("text",x=7,y=28,label="r2=0.53")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("blue","red"))+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\boxrainfall.jpg")


ggplot(monthFull)+
  geom_point(aes(x=rain,y=simP,color=factor(Month)))+
  geom_abline()+
  xlab("Observed Rainy Days")+
  ylab("Simulated of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Rainfall, Monthly")+
  annotate("text",x=5,25,label="r2=0.53")+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\scatterRainfall.jpg")

################detrend################

rainFulldet=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\simRain_full2_det.csv")
rainFulldet=rainFulldet[,c(4:11,13)]
colnames(rainFulldet)=c("PRCP","Year","Month","Day","daynum","mP","sdP",
                        "V8","simP")
monthFull=group_by(rainFulldet,Year,Month) %>%
  summarize(sP2=sd(simP,na.rm=TRUE),rain=sum(PRCP),
            simP=sum(simP),sdR=sd(PRCP,na.rm=TRUE))

yearFull=group_by(rainFulldet,Year) %>%
  summarize(sP=sd(simP),rain=sum(PRCP),simP=sum(simP),
            sdR=sd(PRCP))

rsq=function(x,y) cor(x,y)^2
rsq(monthFull$simP,monthFull$rain)
rsq(yearFull$simP,yearFull$rain)
rsq(rainFulldet$simP,rainFulldet$PRCP)
rsq(na.omit(monthFull$sP2),na.omit(monthFull$sdR))
rsq(na.omit(yearFull$sP),na.omit(yearFull$sdR))

monthFull$month2=monthFull$Month+.5
ggplot(monthFull)+
  geom_boxplot(aes(x=month2,y=rain,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=simP,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Rainfall (mm)")+
  ggtitle("Simulated v Observed Amount of Rainfall, Monthly")+
  annotate("text",x=7,y=28,label="r2=0.61")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("purple","orange"))#+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\boxrainfall_det.jpg")


ggplot(monthFull)+
  geom_point(aes(x=rain,y=simP,color=factor(Month)))+
  geom_abline()+
  xlab("Observed Rainfall (mm)")+
  ylab("Simulated Rainfall (mm)")+
  ggtitle("Simulated v Observed Amount of Rainfall, Monthly")+
  annotate("text",x=3,18,label="r2=0.61")#+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\scatterRainfall_det.jpg")

ggplot(monthFull)+
  geom_density(aes(x=simP,color="simulated",fill="simulated"),size=1.5,alpha=1)+
  geom_density(aes(x=rain,color="observed",fill="observed"),size=1,alpha=.6)+
  guides(color=FALSE)+
  facet_wrap(~Month)+
  xlab("Cumulative Rainfall (mm)")#+
  ggtitle("Density of Simulated vs. Observed Cumulative Monthly Rainfall")

ggplot(monthFull)+
  geom_histogram(aes(x=rain,color="rain",fill="rain"),alpha=.8,bins=20)+
  geom_histogram(aes(x=simP,color="simP",fill="simP"),alpha=.4,bins=20)+
  xlab("simulated rainfall (in)")+
  ylab("rainfall (in)")+
  guides(color=FALSE)+
  ggtitle("Histogram Simulated v Observed, Monthly Sums")

  
####so any cross correlations to maintain? 
eugData=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\eugHistWeather_Clean.csv")
eugIrrad=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\Solar_data_GHI_input.csv")
eugIrrad=eugIrrad[,c(2:5,8)]

eugIrrad=group_by(eugIrrad,Year,Month,Day) %>%
  summarize(GHI=sum(Site2))

RFD=rainFulldet[,c(2:4,9)]
colnames(RFD)=c("Year","Month","Day","simP")
eugD=eugData[,-c(1,10)]
combSim=full_join(RFD,eugD)
combSim=full_join(combSim,eugIrrad)
combSim=combSim[,-c(1:3)]
corrs=cor(combSim,use="complete.obs")
library(corrplot)
corrplot(corrs,method="color")
################detrended and using lnorm instead of gamma########################

################detrend################
rainFulldetL=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\simRain_full2_det_lnorm.csv")
rainFulldetL=rainFulldet[,c(5:12,14)]
colnames(rainFulldetL)=c("PRCP","Year","Month","Day","daynum","mP","sdP",
                        "V8","simP")
monthFull=group_by(rainFulldetL,Year,Month) %>%
  summarize(rain=sum(PRCP),simP=sum(simP))

yearFull=group_by(rainFulldetL,Year) %>%
  summarize(rain=sum(PRCP),simP=sum(simP))

rsq=function(x,y) cor(x,y)^2
rsq(monthFull$simP,monthFull$rain)
rsq(yearFull$simP,yearFull$rain)
rsq(rainFulldetL$simP,rainFulldetL$PRCP)

monthFull$month2=monthFull$Month+.5
ggplot(monthFull)+
  geom_boxplot(aes(x=month2,y=rain,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=simP,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Rainfall (mm)")+
  ggtitle("Simulated v Observed Amount of Rainfall, Monthly")+
  annotate("text",x=7,y=28,label="r2=0.53")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("blue","red"))+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\boxrainfall_detL.jpg")


ggplot(monthFull)+
  geom_point(aes(x=rain,y=simP,color=factor(Month)))+
  geom_abline()+
  xlab("Observed Rainy Days")+
  ylab("Simulated of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Rainfall, Monthly")+
  annotate("text",x=5,25,label="r2=0.58")+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\scatterRainfall_detL.jpg")


#######################WITH SIMULATED TEMPERATURES##############################
rainStateValSyn=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\rainStates_validation_synT.csv")
actualV=rainStateValSyn[,16]
predictV=rainStateValSyn[,39]
compareV=cbind(actualV,predictV)

rainStateSyn=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\rainStates_synT.csv")
actual=rainStateSyn[,16]
predict=rainStateSyn[,39]
compare=cbind(actual,predict)
colnames(compare)=c("val","sim")

corrsDataV=rainStateValSyn[,c(4,6,8,11,12,16,17,39)]
corrsData=rainStateSyn[,c(4,6,8,11,12,16,17,39)]
colnames(corrsDataV)=c("simMin","simMax","wind","TMAX","TMIN","state",
                       "GHI","simState")
colnames(corrsData)=c("simMin","simMax","wind","TMAX","TMIN","state",
                       "GHI","simState")

corrSynT=cor(corrsData)
corrSynTV=cor(corrsDataV)

library(corrplot)
corrplot(corrSynT,method="color")
corrplot(corrSynTV,method="color")

monthlyRain=group_by(rainStateSyn,Year,Month) %>%
  summarize(rain=sum(rain),simState=sum(X0))

monthlyRain$month2=monthlyRain$Month+.5

yearlyRain=group_by(rainStateSyn,Year)%>%
  summarize(rain=sum(rain),simState=sum(X0))

rrse(monthlyRain$simState,monthlyRain$rain)
rmse(monthlyRain$simState,monthlyRain$rain)

rrse(yearlyRain$simState,yearlyRain$rain)
rmse(yearlyRain$simState,yearlyRain$rain)

rsq=function(x,y) cor(x,y)^2
rsq(monthlyRain$simState,monthlyRain$rain)

ggplot(monthlyRain)+
  geom_boxplot(aes(x=month2,y=rain,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=simState,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Number of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Wet Days, Monthly")+
  annotate("text",x=7,y=28,label="r2=0.76")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("purple","orange3"))+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\boxWetDays_synT_full.jpg")


ggplot(monthlyRain)+
  geom_point(aes(x=rain,y=simState,color=factor(Month)))+
  geom_abline()+
  xlab("Observed Rainy Days")+
  ylab("Simulated of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Wet Days, Monthly")+
  annotate("text",x=5,25,label="r2=0.76")+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\boxWetDays_val_synT_full.jpg")

rsq(yearlyRain$simState,yearlyRain$rain)
ggplot(yearlyRain)+
  geom_point(aes(x=rain,y=simState))+
  geom_abline()+
  ggtitle("Simulated v Observed Amount of Wet Days, Annually")+
  annotate("text",90,150,label="r2=0.92")+
  ggsave("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\plots\\boxWetDays_val_synT.jpg")

##dry spells##
compare=compare[,-1]
lenDryA=0
spellsDryA=0
lenDrySpellA=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,1]==0) {
    if (compare[i,1]==compare[(i-1),1]) {
      lenDryA=lenDryA+1
    } else {
      lenDrySpellA[i]=lenDryA
      lenDryA=0
      spellsDryA=spellsDryA+1
    }
    avgLenDryAct=sum(lenDrySpellA,na.rm=TRUE)/spellsDryA
  }
}

lenDryT=0
spellsDryT=0
lenDrySpellT=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,2]==0) {
    if (compare[i,2]==compare[(i-1),2]) {
      lenDryT=lenDryT+1
    } else {
      lenDrySpellT[i]=lenDryT
      lenDryT=0
      spellsDryT=spellsDryT+1
    }
    avgLenDryTest=sum(lenDrySpellT,na.rm=TRUE)/spellsDryT
    
  }
}

##wet spells##
lenWetA=0
spellsWetA=0
lenWetSpellA=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,2]==1) {
    if (compare[i,1]==compare[(i-1),1]) {
      lenWetA=lenWetA+1
    } else {
      lenWetSpellA[i]=lenWetA
      lenWetA=0
      spellsWetA=spellsWetA+1
    }
    avgLenWetAct=sum(lenWetSpellA,na.rm=TRUE)/spellsWetA
  }
}

lenWetT=0
spellsWetT=0
lenWetSpellT=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,2]==1) {
    if (compare[i,2]==compare[(i-1),2]) {
      lenWetT=lenWetT+1
    } else {
      lenWetSpellT[i]=lenWetT
      lenWetT=0
      spellsWetT=spellsWetT+1
    }
    avgLenWetTest=sum(lenWetSpellT,na.rm=TRUE)/spellsWetT
  }
}


avgLenWetAct
avgLenWetTest
avgLenDryAct
avgLenDryTest

##dry spells##
lenDryA=0
spellsDryA=0
lenDrySpellA=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compareV)) {
  if (compareV[i,1]==0) {
    if (compareV[i,1]==compareV[(i-1),2]) {
      lenDryA=lenDryA+1
    } else {
      lenDrySpellA[i]=lenDryA
      lenDryA=0
      spellsDryA=spellsDryA+1
    }
    avgLenDryActV=sum(lenDrySpellA,na.rm=TRUE)/spellsDryA
  }
}

lenDryT=0
spellsDryT=0
lenDrySpellT=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compareV)) {
  if (compareV[i,2]==0) {
    if (compareV[i,2]==compareV[(i-1),2]) {
      lenDryT=lenDryT+1
    } else {
      lenDrySpellT[i]=lenDryT
      lenDryT=0
      spellsDryT=spellsDryT+1
    }
    avgLenDryTestV=sum(lenDrySpellT,na.rm=TRUE)/spellsDryT
    
  }
}

##wet spells##
lenWetA=0
spellsWetA=0
lenWetSpellA=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compareV)) {
  if (compareV[i,1]==1) {
    if (compareV[i,1]==compareV[(i-1),1]) {
      lenWetA=lenWetA+1
    } else {
      lenWetSpellA[i]=lenWetA
      lenWetA=0
      spellsWetA=spellsWetA+1
    }
    avgLenWetActV=sum(lenWetSpellA,na.rm=TRUE)/spellsWetA
  }
}

lenWetT=0
spellsWetT=0
lenWetSpellT=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compareV)) {
  if (compareV[i,2]==1) {
    if (compareV[i,2]==compareV[(i-1),2]) {
      lenWetT=lenWetT+1
    } else {
      lenWetSpellT[i]=lenWetT
      lenWetT=0
      spellsWetT=spellsWetT+1
    }
    avgLenWetTestV=sum(lenWetSpellT,na.rm=TRUE)/spellsWetT
  }
}


avgLenWetActV
avgLenWetTestV
avgLenDryActV
avgLenDryTestV

avgLenWetAct
avgLenWetTest
avgLenDryAct
avgLenDryTest





