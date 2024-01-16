rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(MASS)

salem1=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\simRainSal.csv")

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
paramJul=fitdistr(salem$PRCP[salem$Month==6],densfun="gamma")
paramAug=fitdistr(salem$PRCP[salem$Month==6],densfun="gamma")
paramSep=fitdistr(salem$PRCP[salem$Month==9],densfun="gamma")
paramOct=fitdistr(salem$PRCP[salem$Month==10],densfun="gamma")
paramNov=fitdistr(salem$PRCP[salem$Month==11],densfun="gamma")
paramDec=fitdistr(salem$PRCP[salem$Month==12],densfun="gamma")

salem$simP=0
library(RGeode)
for (i in 1:nrow(salem)) {
  if (salem[i,23]==1) {
    salem[i,38]=rgammatr(1,paramJan$estimate["shape"],paramJan$estimate["rate"],c(.62,5))
  }
   else if (salem[i,20]==2) {
    salem[i,38]=rgammatr(1,paramFeb$estimate["shape"],paramFeb$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==3) {
    salem[i,38]=rgammatr(1,paramMar$estimate["shape"],paramMar$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==4) {
    salem[i,38]=rgammatr(1,paramApr$estimate["shape"],paramApr$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==5) {
    salem[i,38]=rgammatr(1,paramMay$estimate["shape"],paramMay$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==6) {
    salem[i,38]=rgammatr(1,paramJun$estimate["shape"],paramJun$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==7) {
    salem[i,38]=rgammatr(1,paramJul$estimate["shape"],paramJul$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==8) {
    salem[i,38]=rgammatr(1,paramAug$estimate["shape"],paramAug$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==9) {
    salem[i,38]=rgammatr(1,paramSep$estimate["shape"],paramSep$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==10) {
    salem[i,38]=rgammatr(1,paramOct$estimate["shape"],paramOct$estimate["rate"],c(.62,5))
  }
  else if (salem[i,20]==11) {
    salem[i,38]=rgammatr(1,paramNov$estimate["shape"],paramNov$estimate["rate"],c(.62,5))
  }
  else {
    salem[i,38]=rgammatr(1,paramDec$estimate["shape"],paramDec$estimate["rate"],c(.62,5))
  }
}

simMon=group_by(salem,Year,Month)%>%
  summarize(simp=sum(simP),prcp=sum(PRCP),
            varSim=var(simP),varP=var(PRCP),
            sdS=sd(simP),sdP=sd(PRCP))

simMon$mon2=simMon$Month+.5

rsq=function(x,y) cor(x,y)^2
rsq(simMon$prcp,simMon$simp)

ggplot(simMon)+
  geom_boxplot(aes(x=mon2,y=simp,group=Month,fill="simP"),
               outlier.shape=1,width=.4)+
  geom_boxplot(aes(x=Month,y=prcp,group=Month,fill="obs"),
               outlier.shape=1,width=.4)+
  xlab("Month")+
  ylab("Rainfall (in)")+
  ggtitle("Boxplot of Simulated versus Observed Rainfall (historical state)")

ggplot(simMon)+
  geom_point(aes(x=prcp,y=simp,color=factor(Month)))+geom_abline()+facet_wrap(~Month)

ggplot(salem)+
  geom_point(aes(x=PRCP,y=simP)) ##still not good xD

###########################################################
###less intense 
salem2=dplyr::filter(salem1,PRCP<=.62)
salem2=dplyr::filter(salem2,PRCP>0.02) #throws an error any lower than this

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

salem2$simP=0
library(RGeode)
for (i in 1:nrow(salem2)) {
  if (salem2[i,20]==1) {
    salem2[i,38]=rgammatr(1,paramJan2$estimate["shape"],paramJan2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==2) {
    salem2[i,38]=rgammatr(1,paramFeb2$estimate["shape"],paramFeb2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==3) {
    salem2[i,38]=rgammatr(1,paramMar2$estimate["shape"],paramMar2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==4) {
    salem2[i,38]=rgammatr(1,paramApr2$estimate["shape"],paramApr2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==5) {
    salem2[i,38]=rgammatr(1,paramMay2$estimate["shape"],paramMay2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==6) {
    salem2[i,38]=rgammatr(1,paramJun2$estimate["shape"],paramJun2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==7) {
    salem2[i,38]=rgammatr(1,paramJul2$estimate["shape"],paramJul2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==8) {
    salem2[i,38]=rgammatr(1,paramAug2$estimate["shape"],paramAug2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==9) {
    salem2[i,38]=rgammatr(1,paramSep2$estimate["shape"],paramSep2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==10) {
    salem2[i,38]=rgammatr(1,paramOct2$estimate["shape"],paramOct2$estimate["rate"],c(.01,.61))
  }
  else if (salem2[i,20]==11) {
    salem2[i,38]=rgammatr(1,paramNov2$estimate["shape"],paramNov2$estimate["rate"],c(.01,.61))
  }
  else {
    salem2[i,38]=rgammatr(1,paramDec2$estimate["shape"],paramDec2$estimate["rate"],c(.01,.61))
  }
}

simMon2=group_by(salem2,Year,Month)%>%
  summarize(simp=sum(simP),prcp=sum(PRCP),
            varSim=var(simP),varP=var(PRCP),
            sdS=sd(simP),sdP=sd(PRCP))

rsq(simMon2$prcp,simMon2$simp)

simMon2$mon2=simMon2$Month+.5
ggplot(simMon2)+
  geom_boxplot(aes(x=mon2,y=simp,group=Month,fill="simP"),
               outlier.shape=1,width=.4)+
  geom_boxplot(aes(x=Month,y=prcp,group=Month,fill="obs"),
               outlier.shape=1,width=.4)+
  xlab("Month")+
  ylab("Rainfall (in)")+
  ggtitle("Boxplot of Simulated v Observed Rainfall, Mon, Wet")

ggplot(simMon2)+
  geom_point(aes(x=prcp,y=simp,color=factor(Month)))+geom_abline()+facet_wrap(~Month)+
  ggtitle("Monthly Rainfall, Wet Days")

ggplot(simMon2)+
  geom_density(aes(x=prcp))+
  geom_density(aes(x=simp))+
  facet_wrap(~Month)

rsq=function(x,y) cor(x,y)^2
rsq(simMon2$prcp,simMon2$simp)

##overall##
salem1$simP=0

for (i in 1:nrow(salem1)) {
  if (is.na(salem1[i,17])==TRUE) {
    salem1[i,17]=0
  }
  
  
  if (salem1[i,17]<=0.01) {
    salem1[i,38]=0
  } 
  
  
  
  else if (salem1[i,17] <.62) {
    if (salem1[i,20]==1) {
      salem1[i,38]=rgammatr(1,paramJan2$estimate["shape"],paramJan2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==2) {
      salem1[i,38]=rgammatr(1,paramFeb2$estimate["shape"],paramFeb2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==3) {
      salem1[i,38]=rgammatr(1,paramMar2$estimate["shape"],paramMar2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==4) {
      salem1[i,38]=rgammatr(1,paramApr2$estimate["shape"],paramApr2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==5) {
      salem1[i,38]=rgammatr(1,paramMay2$estimate["shape"],paramMay2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==6) {
      salem1[i,38]=rgammatr(1,paramJun2$estimate["shape"],paramJun2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==7) {
      salem1[i,38]=rgammatr(1,paramJul2$estimate["shape"],paramJul2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==8) {
      salem1[i,38]=rgammatr(1,paramAug2$estimate["shape"],paramAug2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==9) {
      salem1[i,38]=rgammatr(1,paramSep2$estimate["shape"],paramSep2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==10) {
      salem1[i,38]=rgammatr(1,paramOct2$estimate["shape"],paramOct2$estimate["rate"],c(.01,.61))
    }
    else if (salem1[i,20]==11) {
      salem1[i,38]=rgammatr(1,paramNov2$estimate["shape"],paramNov2$estimate["rate"],c(.01,.61))
    }
    else {
      salem1[i,38]=rgammatr(1,paramDec2$estimate["shape"],paramDec2$estimate["rate"],c(.01,.61))
    }
    
  } else {
    
    if (salem1[i,20]==1) {
      salem1[i,38]=rgammatr(1,paramJan$estimate["shape"],paramJan$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==2) {
      salem1[i,38]=rgammatr(1,paramFeb$estimate["shape"],paramFeb$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==3) {
      salem1[i,38]=rgammatr(1,paramMar$estimate["shape"],paramMar$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==4) {
      salem1[i,38]=rgammatr(1,paramApr$estimate["shape"],paramApr$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==5) {
      salem1[i,38]=rgammatr(1,paramMay$estimate["shape"],paramMay$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==6) {
      salem1[i,38]=rgammatr(1,paramJun$estimate["shape"],paramJun$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==7) {
      salem1[i,38]=rgammatr(1,paramJul$estimate["shape"],paramJul$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==8) {
      salem1[i,38]=rgammatr(1,paramAug$estimate["shape"],paramAug$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==9) {
      salem1[i,38]=rgammatr(1,paramSep$estimate["shape"],paramSep$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==10) {
      salem1[i,38]=rgammatr(1,paramOct$estimate["shape"],paramOct$estimate["rate"],c(.62,5))
    }
    else if (salem1[i,20]==11) {
      salem1[i,38]=rgammatr(1,paramNov$estimate["shape"],paramNov$estimate["rate"],c(.62,5))
    }
    else {
      salem1[i,38]=rgammatr(1,paramDec$estimate["shape"],paramDec$estimate["rate"],c(.62,5))
    }
  }
}

simMon3=dplyr::group_by(salem1,Year,Month)%>%
  summarize(simp=sum(simP),prcp=sum(PRCP))

simMon3$mon2=simMon3$Month+.5
ggplot(simMon3)+
  geom_boxplot(aes(x=mon2,y=simp,group=Month,fill="simP"),
               outlier.shape=1,width=.4)+
  geom_boxplot(aes(x=Month,y=prcp,group=Month,fill="obs"),
               outlier.shape=1,width=.4)+
  xlab("Month")+
  ylab("Rainfall (in)")+
  ggtitle("Boxplot of Simulated v Observed Rainfall, Mon, Overall")

ggplot(simMon3)+
  geom_point(aes(x=prcp,y=simp,color=factor(Month)))+geom_abline()+facet_wrap(~Month)+
  ggtitle("Monthly Rainfall, Overall")

ggplot(simMon3)+
  geom_density(aes(x=prcp))+
  geom_density(aes(x=simp))+
  facet_wrap(~Month)

rsq=function(x,y) cor(x,y)^2
rsq(simMon3$prcp,simMon3$simp)
