##rainfall amounts
rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
##start to indicate april-september dry and remainder wet?
simData=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\SimRain_gamma_Corv_det.csv")

intense=dplyr::filter(simData,rain==2)
ggplot(intense)+
  geom_histogram(aes(log(detP+1.6)))+facet_wrap(~Month)
##box and whisker plot by month
simMon=group_by(simData,Year,Month)%>%
  summarize(simp=sum(simP),prcp=sum(PRCP),
            varSim=var(simP),varP=var(PRCP),
            sdS=sd(simP),sdP=sd(PRCP))

simMon$mon2=simMon$Month+.5
ggplot(simMon)+
  geom_boxplot(aes(x=mon2,y=simp,group=Month,fill="simP"),
               outlier.shape=1,width=.4)+
  geom_boxplot(aes(x=Month,y=prcp,group=Month,fill="obs"),
               outlier.shape=1,width=.4)+
  xlab("Month")+
  ylab("Rainfall (in)")+
  ggtitle("Boxplot of Simulated versus Observed Rainfall (in)")

ggplot(simMon)+
  geom_density(aes(x=simp,fill="simulated"))+
  geom_density(aes(x=prcp,fill="observed"),alpha=.5)+
  facet_wrap(~Month)

ggplot(simData)+
  geom_density(aes(x=simP,fill="simulated"))+
  geom_density(aes(x=PRCP,fill="observed"),alpha=.5)+
  facet_wrap(~Month)+
  ggtitle("Histogram of Daily Precipitation, Grouped by Month")

ggplot(simMon)+
  geom_point(aes(x=prcp,y=simp,color=factor(Month)))+facet_wrap(~Month)+
  geom_abline()

yearly=group_by(simData,Year)%>%
  summarize(simp=sum(simP),prcp=sum(PRCP),mp=mean(PRCP))

ggplot(yearly)+
  geom_point(aes(x=simp,y=prcp,color=factor(Year)))+geom_abline()

######################
###r2 calculation####

##at the daily level
simData$RSS=0
simData$TSS=0
mean=mean(simData$PRCP)
for (i in 1:nrow(simData)) {
  simData[i,16]=(simData[i,4]-simData[i,15])^2  ##RSS
  simData[i,17]=(simData[i,4]-mean)^2 ##TSS
}

RSS=sum(simData[,16])
TSS=sum(simData[,17])
r2=1-RSS/TSS


##at the monthly level, r2
simMon$RSS=0
simMon$TSS=0
meanMon=mean(simMon$prcp)

for (i in 1:nrow(simMon)){
  simMon[i,6]=(simMon[i,4]-simMon[i,3])^2
  simMon[i,7]=(simMon[i,4]-meanMon)^2  
}

RSS=sum(simMon[,6])
TSS=sum(simMon[,7])
r2_mon=1-RSS/TSS

simMon2=arrange(simMon,Month)
noDec=simMon2[1:717,]
noDec=as.data.frame(noDec)
noDec$simp=as.numeric(noDec$simp)
noDec$prcp=as.numeric(noDec$prcp)

rsq=function(x,y) cor(x,y)^2
rsq(simMon$prcp,simMon$simp)
rsq(simData$PRCP,simData$simP)
rsq(yearly$simp,yearly$prcp)
######################

simStats=group_by(simData,Month)%>%
  summarize(mean=mean(simP),sd=sd(simP),
            median=median(simP))

obsStats=group_by(simData,Month)%>%
  summarize(mean=mean(PRCP),sd=sd(PRCP),
            median=median(PRCP))

ssM=simStats$mean
osM=obsStats$mean

ssMed=simStats$median
osMed=obsStats$median

ssD=simStats$sd
osD=obsStats$sd

rmseMean=rmse(ssM,osM)
rmseMed=rmse(ssMed,osMed)
rmseD=rmse(ssD,osD)

library(MLmetrics)
MAPE(simData$simP,simData$PRCP)
rmse(simData$simP,simData$PRCP)
rmse(simData$PRCP,mean(simData$PRCP))
##lower rmse than just always choosing the mean
##baseline = mean##
cumSim=group_by(simData,Year,Month)%>%
  summarize(simP=sum(simP))

cumObs=group_by(simData,Year,Month)%>%
  summarize(prcp=sum(PRCP))

cumSim=cumSim$simP
cumObs=cumObs$prcp

rmse(cumSim,cumObs)

monthlyData=group_by(simData,Year,Month)%>%
  summarize(simP=sum(simP),obsP=sum(PRCP),
            mSim=mean(simP),mObs=mean(PRCP))

rsq=function(x,y) cor(x,y)^2

rsq(monthlyData$obsP,monthlyData$simP)


ggplot(monthlyData)+
  geom_point(aes(x=obsP,y=simP,color=factor(Month)))+
  facet_wrap(~Month)+
  geom_abline()+
  ggtitle("monthly cumulative precipitation, sim v obs")

rsq(yearly$simP,yearly$obsP)

eug_meds=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\eug_med_gamma.csv") 
ggplot(eug_meds)+
  geom_histogram(aes(x=PRCP),color="purple",fill="purple")+
  geom_histogram(aes(x=simP),color="gold",fill="gold",alpha=.5)+
  facet_wrap(~Month)

meds_mon=group_by(eug_meds,Year,Month)%>%
  summarize(PRCP=sum(PRCP),simp=sum(simP))

meds_mon$Month2=meds_mon$Month+.5
ggplot(meds_mon)+
  geom_boxplot(aes(x=Month,y=PRCP,group=Month,color=factor(Month)),
               outlier.color="purple",outlier.shape=1,width=.4)+
  geom_boxplot(aes(x=Month2,y=simp,group=Month,color=factor(Month)),
               outlier.color="gold",outlier.shape=1,width=.4)

rsq=function(x,y) cor(x,y)^2
rsq(meds_mon$PRCP,meds_mon$simp)
ggplot(meds_mon)+
  geom_point(aes(x=PRCP,y=simp,color=factor(Month)))


eug_hi=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\eug_high_gamma.csv") 
ggplot(eug_hi)+
  geom_histogram(aes(x=PRCP),color="purple",fill="purple")+
  geom_histogram(aes(x=simP),color="gold",fill="gold",alpha=.5)+
  facet_wrap(~Month)

hi_mon=group_by(eug_hi,Year,Month)%>%
  summarize(PRCP=sum(PRCP),simp=sum(simP))

hi_mon$Month2=hi_mon$Month+.5
ggplot(hi_mon)+
  geom_boxplot(aes(x=Month,y=PRCP,group=Month,color=factor(Month)),
               outlier.color="purple",outlier.shape=1,width=.4)+
  geom_boxplot(aes(x=Month2,y=simp,group=Month,color=factor(Month)),
               outlier.color="gold",outlier.shape=1,width=.4)

rsq=function(x,y) cor(x,y)^2
rsq(hi_mon$PRCP,hi_mon$simp)
ggplot(hi_mon)+
  geom_point(aes(x=PRCP,y=simp,color=factor(Month)))
