rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(MASS)
library(RGeode)


salem1=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\simRainSalsyn3.csv")

for (i in 1:nrow(salem1)) {
  if (is.na(salem1[i,17])==TRUE) {
    salem1[i,17]=0
  }
  if (salem1[i,17]<.02) {
    salem1[i,17]=0
  }
}

simR1=matrix(nrow=nrow(salem1),ncol=1)
for (i in 1:nrow(salem1)) {
  if (salem1[i,40]==0) {
    simR1[i,1]=0
  } else {
    simR1[i,1]=1
  }
}

rsq=function(x,y) {cor(x,y)^2}
rsq(simR1[,1],salem1$rain1)

colnames(simR1)="simRain1"
sal=cbind(salem1,simR1)

salMon=dplyr::group_by(sal,Year,Month)%>%
  summarize(rain=sum(rain),simR=sum(simRainType),
            rain1=sum(rain1),simR1=sum(simRain1))

ggplot(salMon)+geom_point(aes(x=rain1,y=simR1,color=factor(Month)))+geom_abline()+
  facet_wrap(~Month)+ggtitle("Simulated v Observed Rainy Days")

rsq(salMon$rain,salMon$simR)

salYear=dplyr::group_by(salem1,Year)%>%
  summarize(rain=sum(rain),simR=sum(simRainType))

ggplot(salYear)+geom_point(aes(x=rain,y=simR))+geom_abline()

ggplot(salem1)+
  geom_histogram(aes(rain,fill="observed"),alpha=1)+facet_wrap(~Month)+
  geom_histogram(aes(simRainType,fill="simulated"),alpha=.5)+
  ggtitle("Histogram of Observed v Simulated Rainy Days")

ggplot(salem1)+
  geom_density(aes(rain,fill="observed"),alpha=1)+facet_wrap(~Month)+
  geom_density(aes(simRainType,fill="simulated"),alpha=.5)

############################################################

salem=dplyr::filter(salem1,PRCP>.62)

paramJan=fitdistr(salem$PRCP[salem$Month==1],densfun="gamma")
paramFeb=fitdistr(salem$PRCP[salem$Month==2],densfun="gamma")
paramMar=fitdistr(salem$PRCP[salem$Month==3],densfun="gamma")
paramApr=fitdistr(salem$PRCP[salem$Month==4],densfun="gamma")
paramMay=fitdistr(salem$PRCP[salem$Month==5],densfun="gamma")
paramJun=fitdistr(salem$PRCP[salem$Month==6],densfun="gamma")
paramJul=fitdistr(c(.62,salem$PRCP[salem$Month==7]),densfun="gamma") ##making same as june because have none
paramAug=fitdistr(c(.62,.63),densfun="gamma") ##same
paramSep=fitdistr(salem$PRCP[salem$Month==9],densfun="gamma")
paramOct=fitdistr(salem$PRCP[salem$Month==10],densfun="gamma")
paramNov=fitdistr(salem$PRCP[salem$Month==11],densfun="gamma")
paramDec=fitdistr(salem$PRCP[salem$Month==12],densfun="gamma")

salem1$simP=0
for (i in 1:nrow(salem1)) {
  if (salem1[i,40]==2) {
    if (salem1[i,22]==1) {
      salem1[i,41]=rgammatr(1,paramJan$estimate["shape"],paramJan$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==2) {
      salem1[i,41]=rgammatr(1,paramFeb$estimate["shape"],paramFeb$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==3) {
      salem1[i,41]=rgammatr(1,paramMar$estimate["shape"],paramMar$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==4) {
      salem1[i,41]=rgammatr(1,paramApr$estimate["shape"],paramApr$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==5) {
      salem1[i,41]=rgammatr(1,paramMay$estimate["shape"],paramMay$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==6) {
      salem1[i,41]=rgammatr(1,paramJun$estimate["shape"],paramJun$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==7) {
      salem1[i,41]=rgammatr(1,paramJul$estimate["shape"],paramJul$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==8) {
      salem1[i,41]=rgammatr(1,paramAug$estimate["shape"],paramAug$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==9) {
      salem1[i,41]=rgammatr(1,paramSep$estimate["shape"],paramSep$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==10) {
      salem1[i,41]=rgammatr(1,paramOct$estimate["shape"],paramOct$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,22]==11) {
      salem1[i,41]=rgammatr(1,paramNov$estimate["shape"],paramNov$estimate["rate"],c(.62,5))
    }
    else {
      salem1[i,41]=rgammatr(1,paramDec$estimate["shape"],paramDec$estimate["rate"],c(.62,5))
    }
  } else {
    salem1[i,41]=0
  }
}
###########################################################
###less intense 
salem2=dplyr::filter(salem1,PRCP<=.62)
salem2=dplyr::filter(salem2,PRCP>0.01) #0.01 is classified as a zero PRCP day 

paramJan2=fitdistr(salem2$PRCP[salem2$Month==1],densfun="gamma",lower=0)
paramFeb2=fitdistr(salem2$PRCP[salem2$Month==2],densfun="gamma",lower=0)
paramMar2=fitdistr(salem2$PRCP[salem2$Month==3],densfun="gamma",lower=0)
paramApr2=fitdistr(salem2$PRCP[salem2$Month==4],densfun="gamma",lower=0)
paramMay2=fitdistr(salem2$PRCP[salem2$Month==5],densfun="gamma",lower=0)
paramJun2=fitdistr(salem2$PRCP[salem2$Month==6],densfun="gamma",lower=0)
paramJul2=fitdistr(salem2$PRCP[salem2$Month==7],densfun="gamma",lower=0)
paramAug2=fitdistr(salem2$PRCP[salem2$Month==8],densfun="gamma",lower=0)
paramSep2=fitdistr(salem2$PRCP[salem2$Month==9],densfun="gamma",lower=0)
paramOct2=fitdistr(salem2$PRCP[salem2$Month==10],densfun="gamma",lower=0)
paramNov2=fitdistr(salem2$PRCP[salem2$Month==11],densfun="gamma",lower=0)
paramDec2=fitdistr(salem2$PRCP[salem2$Month==12],densfun="gamma",lower=0)

for (i in 1:nrow(salem1)) {
  if (salem1[i,40]==1) {
    if (salem1[i,22]==1) {
      salem1[i,41]=rgammatr(1,paramJan2$estimate["shape"],paramJan2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==2) {
      salem1[i,41]=rgammatr(1,paramFeb2$estimate["shape"],paramFeb2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==3) {
      salem1[i,41]=rgammatr(1,paramMar2$estimate["shape"],paramMar2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==4) {
      salem1[i,41]=rgammatr(1,paramApr2$estimate["shape"],paramApr2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==5) {
      salem1[i,41]=rgammatr(1,paramMay2$estimate["shape"],paramMay2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==6) {
      salem1[i,41]=rgammatr(1,paramJun2$estimate["shape"],paramJun2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==7) {
      salem1[i,41]=rgammatr(1,paramJul2$estimate["shape"],paramJul2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==8) {
      salem1[i,41]=rgammatr(1,paramAug2$estimate["shape"],paramAug2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==9) {
      salem1[i,41]=rgammatr(1,paramSep2$estimate["shape"],paramSep2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==10) {
      salem1[i,41]=rgammatr(1,paramOct2$estimate["shape"],paramOct2$estimate["rate"],c(0,.62))
    }
    else if (salem1[i,22]==11) {
      salem1[i,41]=rgammatr(1,paramNov2$estimate["shape"],paramNov2$estimate["rate"],c(0,.62))
    }
    else {
      salem1[i,41]=rgammatr(1,paramDec2$estimate["shape"],paramDec2$estimate["rate"],c(0,.62))
    }
  } else {
    salem1[i,41]=salem1[i,41]
  }
}

simMon=dplyr::group_by(salem1,Year,Month)%>%
  summarize(simp=sum(simP),prcp=sum(PRCP),
            varSim=var(simP),varP=var(PRCP),
            sdS=sd(simP),sdP=sd(PRCP),
            raindays=sum(rain),simdays=sum(simRainType))

simMon$mon2=simMon$Month+.5

simMon$diffObs=0
simMon$diffMean=0
mean=mean(simMon$prcp)
for (i in 1:nrow(simMon)) {
  simMon[i,12]=(simMon[i,4]-simMon[i,3])^2
  simMon[i,13]=(simMon[i,4]-mean)^2
}

sumDObs=sum(simMon[,12])
sumDMean=sum(simMon[,13])
rsq2=1-(sumDObs/sumDMean)
rsq2

rsq=function(x,y) {cor(x,y)^2}
rsq(simMon$simp,simMon$prcp)

ggplot(salem1)+geom_point(aes(x=PRCP,y=simP,color=factor(Month)))

ggplot(simMon)+
  geom_point(aes(x=prcp,y=simp,color=factor(Month)))+
  annotate("text",x=3,y=13,label="r^2=.79")+
  geom_abline()+
  xlab("observed rainfall (in)")+
  ylab("simulated rainfall (in)")+
  ggtitle("Observed versus Historical (sim T)")

ggplot(simMon)+
  geom_point(aes(x=prcp,y=simp,color=factor(Month)))+
  geom_abline()+facet_wrap(~Month)+
  xlab("observed rainfall (in)")+
  ylab("simulated rainfall (in)")+
  ggtitle("Observed versus Historical Monthly Rainfall")

ggplot(salem1)+
  geom_histogram(aes(x=simP,fill="simulated"),alpha=1,bins=60)+
  geom_histogram(aes(x=PRCP,fill="observed"),alpha=.5,bins=60)+
  facet_wrap(~Month)+
  #annotate("text",x=2,y=4000,label="r^2=.79")+
  ggtitle("Observed v Simulated Rainfall (simT)")


ggplot(simMon)+
  geom_boxplot(aes(x=mon2,y=simp,group=Month,fill="simP"),
               outlier.shape=1,width=.4)+
  geom_boxplot(aes(x=Month,y=prcp,group=Month,fill="obs"),
               outlier.shape=1,width=.4)+
  xlab("Month")+
  ylab("Rainfall (in)")+
  annotate("text",x=6,y=10,label="r^2=.79")+
  ggtitle("Simulated versus Observed Rainfall (simT)")

ggplot(simMon)+
  geom_point(aes(x=prcp,y=simp,color=factor(Month)))+geom_abline()+facet_wrap(~Month)

ggplot(simMon)+
  geom_density(aes(x=prcp,fill="observed"))+
  geom_density(aes(x=simp,fill="simulated"),alpha=.5)+
  facet_wrap(~Month)

yearly=dplyr::group_by(salem1,Year)%>%
  summarize(prcp=sum(PRCP),simp=sum(simP))

ggplot(yearly)+
  geom_point(aes(x=prcp,y=simp))+geom_abline()

###maintains correlations?
dayvars=salem1[,c(16,18:20,24,17,41)]
cors=cor(dayvars,use="complete.obs")
library(corrplot)
corrplot(cors,method="color")

###autocorrelation/serial correlation?

obsacf <- acf(salem1$PRCP, plot = FALSE)
obsacf2 <- with(obsacf, data.frame(lag, acf))
simacf <- acf(salem1$simP, plot = FALSE)
simacf2 <- with(simacf, data.frame(lag, acf))

q <- ggplot(data=obsacf2, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")
p <- ggplot(data=simacf2, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")

q+ggtitle("Observed Rainfall Amount ACF")
p+ggtitle("Simulated Rainfall Amount ACF")

#write.csv(salem1,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\salemSimSynT.csv")
