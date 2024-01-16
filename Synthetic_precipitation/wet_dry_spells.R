##prcp plots for eugene
rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(Metrics)

hist_precip1=read.csv("C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Salem_Weather_fulldet.csv")
actual=read.csv("C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\actualSal.csv")
predict=read.csv("C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\predictedSal.csv")
colnames(actual)=c("day","val")
colnames(predict)=c("day","predict")
compare=full_join(actual,predict)

#compare=hist_precip3[,c(1,26,28)]

####################WET AND DRY SPELLS###################
compare=compare[,-1]
lenDryA=1
spellsDryA=0
lenDrySpellA=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,1]==0 & compare[(i-1),1]==0) {
      lenDryA=lenDryA+1
    } else {
      lenDrySpellA[i]=lenDryA
      lenDryA=1
      spellsDryA=spellsDryA+1
    }
    avgLenDryAct=sum(lenDrySpellA,na.rm=TRUE)/spellsDryA
  }


lenDryT=1
spellsDryT=0
lenDrySpellT=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,2]==0 & compare[(i-1),2]) {
      lenDryT=lenDryT+1
    } else {
      lenDrySpellT[i]=lenDryT
      lenDryT=1
      spellsDryT=spellsDryT+1
    }
    avgLenDryTest=sum(lenDrySpellT,na.rm=TRUE)/spellsDryT
}

##wet spells##
lenWetA=1
spellsWetA=0
lenWetSpellA=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,1]==1 | compare[i,1]==2) {
    if (compare[i,1]==compare[(i-1),1]) {
      lenWetA=lenWetA+1
    } else {
      lenWetSpellA[i]=lenWetA
      lenWetA=1
      spellsWetA=spellsWetA+1
    }
    avgLenWetAct=sum(lenWetSpellA,na.rm=TRUE)/spellsWetA
  }
}

lenWetT=1
spellsWetT=0
lenWetSpellT=as.vector(c(rep(0,1000)))
for (i in 2:nrow(compare)) {
  if (compare[i,2]==1 | compare[i,2]==2) {
    if (compare[i,2]==compare[(i-1),2]) {
      lenWetT=lenWetT+1
    } else {
      lenWetSpellT[i]=lenWetT
      lenWetT=1
      spellsWetT=spellsWetT+1
    }
    avgLenWetTest=sum(lenWetSpellT,na.rm=TRUE)/spellsWetT
  }
}


avgLenWetAct
avgLenWetTest
avgLenDryAct
avgLenDryTest

sum(compare$val)
sum(compare$predict)


all=read.csv("C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\simRainSal.csv")

ggplot(all)+
  geom_histogram(aes(rain,fill="historical rain state"))+
  geom_histogram(aes(simRainType,fill="simulated rain state"),alpha=.5)+
  xlab("Rain State")+
  ylab("Frequency")+
  theme_minimal()+  
  guides(fill=guide_legend(element_blank()))+
  ggtitle("Histogram of Rain States (Historical vs. Simulated)")+facet_wrap(~Month)

