rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(coefplot)
library(fitdistrplus)
library(markovchain)
hist_precip1=read.csv("C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Salem_Weather_full.csv")

#hist_precip1=na.omit(hist_precip1)
########gamma parameters##########################
hist_precip=dplyr::filter(hist_precip1,PRCP>.61)

paramJan=fitdistr(hist_precip$PRCP[hist_precip$Month==1],densfun="gamma")
paramFeb=fitdistr(hist_precip$PRCP[hist_precip$Month==2],densfun="gamma")
paramMar=fitdistr(hist_precip$PRCP[hist_precip$Month==3],densfun="gamma")
paramApr=fitdistr(hist_precip$PRCP[hist_precip$Month==4],densfun="gamma")
paramMay=fitdistr(hist_precip$PRCP[hist_precip$Month==5],densfun="gamma")
paramJun=fitdistr(hist_precip$PRCP[hist_precip$Month==6],densfun="gamma")
paramJul=fitdistr(hist_precip$PRCP[hist_precip$Month==7],densfun="gamma") 
paramAug=fitdistr(hist_precip$PRCP[hist_precip$Month==6],densfun="gamma")
paramSep=fitdistr(hist_precip$PRCP[hist_precip$Month==9],densfun="gamma")
paramOct=fitdistr(hist_precip$PRCP[hist_precip$Month==10],densfun="gamma")
paramNov=fitdistr(hist_precip$PRCP[hist_precip$Month==11],densfun="gamma")
paramDec=fitdistr(hist_precip$PRCP[hist_precip$Month==12],densfun="gamma")

#####simulating historic####
hist_precip1$simP=0
for (i in 1:nrow(hist_precip1)) {
  if (hist_precip1[i,26]==2) {
    if (hist_precip1[i,8]==1) {
      hist_precip1[i,27]=rgammatr(1,paramJan$estimate["shape"],paramJan$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==2) {
      hist_precip1[i,27]=rgammatr(1,paramFeb$estimate["shape"],paramFeb$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==3) {
      hist_precip1[i,27]=rgammatr(1,paramMar$estimate["shape"],paramMar$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==4) {
      hist_precip1[i,27]=rgammatr(1,paramApr$estimate["shape"],paramApr$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==5) {
      hist_precip1[i,27]=rgammatr(1,paramMay$estimate["shape"],paramMay$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==6) {
      hist_precip1[i,27]=rgammatr(1,paramJun$estimate["shape"],paramJun$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==7) {
      hist_precip1[i,27]=rgammatr(1,paramJul$estimate["shape"],paramJul$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==8) {
      hist_precip1[i,27]=rgammatr(1,paramAug$estimate["shape"],paramAug$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==9) {
      hist_precip1[i,27]=rgammatr(1,paramSep$estimate["shape"],paramSep$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==10) {
      hist_precip1[i,27]=rgammatr(1,paramOct$estimate["shape"],paramOct$estimate["rate"],c(.62,3))
    }
    else if (hist_precip1[i,8]==11) {
      hist_precip1[i,27]=rgammatr(1,paramNov$estimate["shape"],paramNov$estimate["rate"],c(.62,3))
    }
    else {
      hist_precip1[i,27]=rgammatr(1,paramDec$estimate["shape"],paramDec$estimate["rate"],c(.62,3))
    }
  } else {
    hist_precip1[i,27]=0
  }
}

simp1=dplyr::filter(hist_precip1,simP>.50)

###########################################################
###less intense parameters 
hist_precip2=dplyr::filter(hist_precip1,PRCP<=.61)
hist_precip2=dplyr::filter(hist_precip2,PRCP>0.01) #0.01 is classified as a zero PRCP day 
simp2=data.frame(nrow=nrow(hist_precip2))
paramJan2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==1],densfun="gamma",lower=0)
paramFeb2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==2],densfun="gamma",lower=0)
paramMar2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==3],densfun="gamma",lower=0)
paramApr2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==4],densfun="gamma",lower=0)
paramMay2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==5],densfun="gamma",lower=0)
paramJun2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==6],densfun="gamma",lower=0)
paramJul2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==7],densfun="gamma",lower=0)
paramAug2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==8],densfun="gamma",lower=0)
paramSep2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==9],densfun="gamma",lower=0)
paramOct2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==10],densfun="gamma",lower=0)
paramNov2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==11],densfun="gamma",lower=0)
paramDec2=fitdistr(hist_precip2$PRCP[hist_precip2$Month==12],densfun="gamma",lower=0)

######simulating historic less intense####

for (i in 1:nrow(hist_precip1)) {
  if (hist_precip1[i,26]==1) {
    if (hist_precip1[i,8]==1) {
      hist_precip1[i,27]=rgammatr(1,paramJan2$estimate["shape"],paramJan2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==2) {
      hist_precip1[i,27]=rgammatr(1,paramFeb2$estimate["shape"],paramFeb2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==3) {
      hist_precip1[i,27]=rgammatr(1,paramMar2$estimate["shape"],paramMar2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==4) {
      hist_precip1[i,27]=rgammatr(1,paramApr2$estimate["shape"],paramApr2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==5) {
      hist_precip1[i,27]=rgammatr(1,paramMay2$estimate["shape"],paramMay2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==6) {
      hist_precip1[i,27]=rgammatr(1,paramJun2$estimate["shape"],paramJun2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==7) {
      hist_precip1[i,27]=rgammatr(1,paramJul2$estimate["shape"],paramJul2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==8) {
      hist_precip1[i,27]=rgammatr(1,paramAug2$estimate["shape"],paramAug2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==9) {
      hist_precip1[i,27]=rgammatr(1,paramSep2$estimate["shape"],paramSep2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==10) {
      hist_precip1[i,27]=rgammatr(1,paramOct2$estimate["shape"],paramOct2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else if (hist_precip1[i,8]==11) {
      hist_precip1[i,27]=rgammatr(1,paramNov2$estimate["shape"],paramNov2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
    else {
      hist_precip1[i,27]=rgammatr(1,paramDec2$estimate["shape"],paramDec2$estimate["rate"],c(.02,.61))
      simp2[i,1]=hist_precip1[i,27]
    }
  } else {
    hist_precip1[i,27]=hist_precip1[i,27]
    simp2[i,1]=0
  }
}


###simulated test set
synRain=read.csv("C:\\Users\\rcuppari\\Desktop\\validRain.csv")
synRain=synRain[1:nrow(hist),c(3,4,5,6,7,8,11,12,13)]
hist=hist_precip1[,c(2:5,8,10,26)]
synRain=cbind(synRain,hist)
synRain$mon2=synRain$month+.5

ggplot(synRain)+
  geom_density(aes(x=simRainType,fill="Simulated"))+
  geom_density(aes(x=PRCP,fill="Historical"))+
  facet_wrap(~month)+
  xlab("Month")+
  ylab("Rainy Days")+
  theme_minimal()+
  theme(panel.grid.minor =   element_blank(),
        legend.title = element_blank(),
        text=element_text(family="Sans"))+
  ggtitle("Simulated versus Historical Rainy Days")

synRain$syn_precip=0
library(RGeode)
for (i in 1:nrow(synRain)) {
  
  if (synRain[i,9]==2) {
    
    if (synRain[i,7]==1) {
      synRain[i,18]=rgammatr(1,paramJan$estimate["shape"],paramJan$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==2) {
      synRain[i,18]=rgammatr(1,paramFeb$estimate["shape"],paramFeb$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==3) {
      synRain[i,18]=rgammatr(1,paramMar$estimate["shape"],paramMar$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==4) {
      synRain[i,18]=rgammatr(1,paramApr$estimate["shape"],paramApr$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==5) {
      synRain[i,18]=rgammatr(1,paramMay$estimate["shape"],paramMay$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==6) {
      synRain[i,18]=rgammatr(1,paramJun$estimate["shape"],paramJun$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==7) {
      synRain[i,18]=rgammatr(1,paramJul$estimate["shape"],paramJul$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==8) {
      synRain[i,18]=rgammatr(1,paramAug$estimate["shape"],paramAug$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==9) {
      synRain[i,18]=rgammatr(1,paramSep$estimate["shape"],paramSep$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==10) {
      synRain[i,18]=rgammatr(1,paramOct$estimate["shape"],paramOct$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (synRain[i,7]==7) {
      synRain[i,18]=rgammatr(1,paramNov$estimate["shape"],paramNov$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else {
      synRain[i,18]=rgammatr(1,paramDec$estimate["shape"],paramDec$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
  } 
  
  else if (synRain[i,9]==1) {
    if (synRain[i,7]==1) {
      synRain[i,18]=rgammatr(1,paramJan2$estimate["shape"],paramJan2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==2) {
      synRain[i,18]=rgammatr(1,paramFeb2$estimate["shape"],paramFeb2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==3) {
      synRain[i,18]=rgammatr(1,paramMar2$estimate["shape"],paramMar2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==4) {
      synRain[i,18]=rgammatr(1,paramApr2$estimate["shape"],paramApr2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==5) {
      synRain[i,18]=rgammatr(1,paramMay2$estimate["shape"],paramMay2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==6) {
      synRain[i,18]=rgammatr(1,paramJun2$estimate["shape"],paramJun2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==7) {
      synRain[i,18]=rgammatr(1,paramJul2$estimate["shape"],paramJul2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==8) {
      synRain[i,18]=rgammatr(1,paramAug2$estimate["shape"],paramAug2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==9) {
      synRain[i,18]=rgammatr(1,paramSep2$estimate["shape"],paramSep2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==10) {
      synRain[i,18]=rgammatr(1,paramOct2$estimate["shape"],paramOct2$estimate["rate"],c(.02,.61))
      
    }
    else if (synRain[i,7]==7) {
      synRain[i,18]=rgammatr(1,paramNov2$estimate["shape"],paramNov2$estimate["rate"],c(.02,.61))
      
    }
    else {
      synRain[i,18]=rgammatr(1,paramDec2$estimate["shape"],paramDec2$estimate["rate"],c(.02,.61))
      
    }
  } 
  
  else {
    synRain[i,18]=0
  }
  if (synRain[i,18]<0) {
    synRain[i,18]=0
  }
}

synRain$season=1
for (i in 1:nrow(synRain)) {
  if (synRain[i,7]==12 | synRain[i,7]==1 | synRain[i,7]==2 ) {
    synRain[i, 19]=1
  } else if (synRain[i,7]==3 | synRain[i,7]==4 | synRain[i,7]==5) {
    synRain[i,19]=2
  } else if (synRain[i,7]==6 | synRain[i,7]==7 | synRain[i,7]==8) {
    synRain[i,19]=3
  } else {
    synRain[i,19]=4
  }
} 

synRain$sea2=synRain$season+.5
ggplot(synRain)+
  geom_boxplot(aes(x=sea2,y=syn_precip,group=season,fill="Simulated"),width=.4)+
  geom_boxplot(aes(x=season,y=PRCP,group=season,fill="Historical"),width=.4)+
  xlab("Season")+
  ylab("Rainfall (in)")+
  theme_minimal()+
  theme(panel.grid.minor =   element_blank(),
        legend.position = "none")+
  ggtitle("Simulated versus Historical Rainfall")+
  scale_x_continuous(breaks=c(1,2,3,4),
                     labels=c("Winter","Spring","Summer","Fall"))
  

#######################

simMon=dplyr::group_by(hist_precip1,Year,Month)%>%
  summarize(simp=sum(simP),prcp=sum(PRCP),
            varSim=var(simP),varP=var(PRCP),
            sdS=sd(simP),sdP=sd(PRCP),
            raindays=sum(rain))

simMon$mon2=simMon$Month+.5

year=dplyr::group_by(hist_precip1,Year)%>%
  summarize(simp=sum(simP),prcp=sum(PRCP))

ggplot(year)+geom_point(aes(x=prcp,y=simp,color=factor(Year)))+geom_abline()

ggplot(simMon)+
  geom_point(aes(x=prcp,y=simp,color=factor(Month)))+
  facet_wrap(~Month)+
  annotate("text",x=3,y=13,label="r^2=.91")+
  geom_abline()+
  xlab("observed rainfall (in)")+
  ylab("simulated rainfall (in)")+
  ggtitle("Observed versus Predicted Monthly Rainfall")

ggplot(simMon)+
  geom_density(aes(prcp,fill="actual"))+
  geom_density(aes(simp,fill="predicted"),alpha=.5)+
  facet_wrap(~Month)+
  ggtitle("Observed versus Predicted Monthly Rainfall Densities")

ggplot(simMon)+
  geom_boxplot(aes(x=mon2,y=simp,group=Month,fill="simP"),
               outlier.shape=1,width=.4)+
  geom_boxplot(aes(x=Month,y=prcp,group=Month,fill="obs"),
               outlier.shape=1,width=.4)+
  xlab("Month")+
  ylab("Rainfall (in)")+
  annotate("text",x=6,y=10,label="r^2=.91")+
  theme_minimal()+
  ggtitle("Simulated versus Observed Rainfall (in)")

mean=mean(simMon$prcp)
simMon$obs=0
simMon$mObs=0
for (i in 1:nrow(simMon)) {
  simMon[i,11]=(simMon[i,4]-simMon[i,3])^2
  simMon[i,12]=(simMon[i,4]-mean)^2
}
r2=1-(sum(simMon$obs)/sum(simMon$mObs))
(cor(simMon$prcp,simMon$simp))^2

#############markov chain##########
janP=filter(hist_precip1,Month==1)
febP=filter(hist_precip1,Month==2)
marP=filter(hist_precip1,Month==3)
aprP=filter(hist_precip1,Month==4)
mayP=filter(hist_precip1,Month==5)
junP=filter(hist_precip1,Month==6)
julP=filter(hist_precip1,Month==7)
augP=filter(hist_precip1,Month==8)
sepP=filter(hist_precip1,Month==9)
octP=filter(hist_precip1,Month==10)
novP=filter(hist_precip1,Month==11)
decP=filter(hist_precip1,Month==12)

transProbs=read.csv("E:\\Research\\SalTransProbs.csv")
transJan=matrix(c(transProbs[1,11],transProbs[1,10],transProbs[1,9],
                  transProbs[1,3],transProbs[1,5],transProbs[1,7],
                  transProbs[1,4],transProbs[1,6],transProbs[1,8]),
                nrow=3,ncol=3)

transFeb=matrix(c(transProbs[2,11],transProbs[2,10],transProbs[2,9],
                  transProbs[2,3],transProbs[2,5],transProbs[2,7],
                  transProbs[2,4],transProbs[2,6],transProbs[2,8]),
                nrow=3,ncol=3)

transMar=matrix(c(transProbs[3,11],transProbs[3,10],transProbs[3,9],
                  transProbs[3,3],transProbs[3,5],transProbs[3,7],
                  transProbs[3,4],transProbs[3,6],transProbs[3,8]),
                nrow=3,ncol=3)

transApr=matrix(c(transProbs[4,11],transProbs[4,10],transProbs[4,9],
                  transProbs[4,3],transProbs[4,5],transProbs[4,7],
                  transProbs[4,4],transProbs[4,6],transProbs[4,8]),
                nrow=3,ncol=3)

transMay=matrix(c(transProbs[5,11],transProbs[5,10],transProbs[5,9],
                  transProbs[5,3],transProbs[5,5],transProbs[5,7],
                  transProbs[5,4],transProbs[5,6],transProbs[5,8]),
                nrow=3,ncol=3)

transJun=matrix(c(transProbs[6,11],transProbs[6,10],transProbs[6,9],
                  transProbs[6,3],transProbs[6,5],transProbs[6,7],
                  transProbs[6,4],transProbs[6,6],transProbs[6,8]),
                nrow=3,ncol=3)

transJul=matrix(c(transProbs[7,11],transProbs[7,10],transProbs[7,9],
                  transProbs[7,3],transProbs[7,5],transProbs[7,7],
                  transProbs[7,4],transProbs[7,6],transProbs[7,8]),
                nrow=3,ncol=3)

transAug=matrix(c(transProbs[8,11],transProbs[8,10],transProbs[8,9],
                  transProbs[8,3],transProbs[8,5],transProbs[8,7],
                  transProbs[8,4],transProbs[8,6],transProbs[8,8]),
                nrow=3,ncol=3)

transSep=matrix(c(transProbs[9,11],transProbs[9,10],transProbs[9,9],
                  transProbs[9,3],transProbs[9,5],transProbs[9,7],
                  transProbs[9,4],transProbs[9,6],transProbs[9,8]),
                nrow=3,ncol=3)

transOct=matrix(c(transProbs[10,11],transProbs[10,10],transProbs[10,9],
                  transProbs[10,3],transProbs[10,5],transProbs[10,7],
                  transProbs[10,4],transProbs[10,6],transProbs[10,8]),
                nrow=3,ncol=3)

transNov=matrix(c(transProbs[11,11],transProbs[11,10],transProbs[11,9],
                  transProbs[11,3],transProbs[11,5],transProbs[11,7],
                  transProbs[11,4],transProbs[11,6],transProbs[11,8]),
                nrow=3,ncol=3)

transDec=matrix(c(transProbs[12,11],transProbs[12,10],transProbs[12,9],
                  transProbs[12,3],transProbs[12,5],transProbs[12,7],
                  transProbs[12,4],transProbs[12,6],transProbs[12,8]),
                nrow=3,ncol=3)

colnames(transJan)=c("dry","wet","vwet")
colnames(transFeb)=c("dry","wet","vwet")
colnames(transMar)=c("dry","wet","vwet")
colnames(transApr)=c("dry","wet","vwet")
colnames(transMay)=c("dry","wet","vwet")
colnames(transJun)=c("dry","wet","vwet")
colnames(transJul)=c("dry","wet","vwet")
colnames(transAug)=c("dry","wet","vwet")
colnames(transSep)=c("dry","wet","vwet")
colnames(transOct)=c("dry","wet","vwet")
colnames(transNov)=c("dry","wet","vwet")
colnames(transDec)=c("dry","wet","vwet")


mcJan=markovchainFit(janP$rain,confidencelevel=.99)
mcFeb=markovchainFit(febP$rain,confidencelevel=.99)
mcMar=markovchainFit(marP$rain,confidencelevel=.99)
mcApr=markovchainFit(aprP$rain,confidencelevel=.99)
mcMay=markovchainFit(mayP$rain,confidencelevel=.99)
mcJun=markovchainFit(junP$rain,confidencelevel=.99)
mcJul=markovchainFit(julP$rain,confidencelevel=.99)
mcAug=markovchainFit(augP$rain,confidencelevel=.99)
mcSep=markovchainFit(sepP$rain,confidencelevel=.99)
mcOct=markovchainFit(octP$rain,confidencelevel=.99)
mcNov=markovchainFit(novP$rain,confidencelevel=.99)
mcDec=markovchainFit(decP$rain,confidencelevel=.99)

#################hist##############
hist_precip3=na.omit(hist_precip1)
hist_precip3$simState=0
hist_precip3[1,28]=1
for (i in 2:nrow(hist_precip3)) { 
  if (hist_precip3[i,16]<=31) {
    initialPR=mcJan$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=59) {
    initialPR=mcFeb$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=90) {
    initialPR=mcMar$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=120)  {
    initialPR=mcApr$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=151)  {
    initialPR=mcMay$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=181)  {
    initialPR=mcJun$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=212)  {
    initialPR=mcJul$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=243)  {
    initialPR=mcAug$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=273)  {
    initialPR=mcSep$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=304)  {
    initialPR=mcOct$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (hist_precip3[i,16]<=334)  {
    initialPR=mcNov$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else {
    initialPR=mcDec$estimate
    if (hist_precip3[(i-1),28]==1) { 
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (hist_precip3[(i-1),28]==2) {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      hist_precip3[i,28]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
}  

ggplot(hist_precip3)+
  geom_density(aes(simState,fill="sim"))+
  geom_density(aes(rain,fill="act"),alpha=.5)+
  facet_wrap(~Month)+
  ggtitle("Simulated State with Markov Chain")

####using my transition matrix###
#mcJan2=new("markovchain",states=c("dry","wet","vwet"),
#           transitionMatrix=transJan,name="January")

####################################
#######syn weather simulation#######
syn_weather=read.csv("C:\\Users\\rcuppari\\Desktop\\syn_weather150yr_pstates_ownTrendFinal.csv")

for (i in 2:nrow(syn_weather)) { 
  if (syn_weather[i,2]<=31) {
    initialPR=mcJan$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=59) {
    initialPR=mcFeb$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=90) {
    initialPR=mcMar$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=120)  {
    initialPR=mcApr$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=151)  {
    initialPR=mcMay$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=181)  {
    initialPR=mcJun$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=212)  {
    initialPR=mcJul$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=243)  {
    initialPR=mcAug$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=273)  {
    initialPR=mcSep$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=304)  {
    initialPR=mcOct$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else if (syn_weather[i,2]<=334)  {
    initialPR=mcNov$estimate
    if (syn_weather[(i-1),13]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
  else {
    initialPR=mcDec$estimate
    if (syn_weather[(i-1),2]==1) { 
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[2,1],initialPR[2,2],initialPR[2,3]))
    } else if (syn_weather[(i-1),13]==2) {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[3,1],initialPR[3,2],initialPR[3,3]))
    } else {
      syn_weather[i,13]=sample(x=c(0,1,2),1,replace=T,prob=c(initialPR[1,1],initialPR[1,2],initialPR[1,3]))
    }
  }
  
}  


#syn_weather=syn_weather[,-1]
ggplot(syn_weather)+
  geom_density(aes(simRainType,color="sim"))+facet_wrap(~month)+
  ggtitle("sim rain days")
ggplot(hist_precip1)+
  geom_density(aes(rain,color="act"))+facet_wrap(~Month)+
  ggtitle("hist rain days")


syn_weather=read.csv("C:\\Users\\rcuppari\\Desktop\\syn_weather150yr_pstates_ownTrendFinal.csv")
syn_weather$syn_precip=0
#syn_weather=syn_weather[,c(1:19,22,21)]

library(RGeode)
for (i in 1:nrow(syn_weather)) {
  
  if (syn_weather[i,13]==2) {
    
    if (syn_weather[i,11]==1) {
      syn_weather[i,21]=rgammatr(1,paramJan$estimate["shape"],paramJan$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==2) {
      syn_weather[i,21]=rgammatr(1,paramFeb$estimate["shape"],paramFeb$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==3) {
      syn_weather[i,21]=rgammatr(1,paramMar$estimate["shape"],paramMar$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==4) {
      syn_weather[i,21]=rgammatr(1,paramApr$estimate["shape"],paramApr$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==5) {
      syn_weather[i,21]=rgammatr(1,paramMay$estimate["shape"],paramMay$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==6) {
      syn_weather[i,21]=rgammatr(1,paramJun$estimate["shape"],paramJun$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==7) {
      syn_weather[i,21]=rgammatr(1,paramJul$estimate["shape"],paramJul$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==8) {
      syn_weather[i,21]=rgammatr(1,paramAug$estimate["shape"],paramAug$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==9) {
      syn_weather[i,21]=rgammatr(1,paramSep$estimate["shape"],paramSep$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==10) {
      syn_weather[i,21]=rgammatr(1,paramOct$estimate["shape"],paramOct$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else if (syn_weather[i,11]==11) {
      syn_weather[i,21]=rgammatr(1,paramNov$estimate["shape"],paramNov$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
    else {
      syn_weather[i,21]=rgammatr(1,paramDec$estimate["shape"],paramDec$estimate["rate"],c(.62,(max(hist_precip1$PRCP)+.5)))
    }
  } 
  
  else if (syn_weather[i,13]==1) {
    if (syn_weather[i,11]==1) {
      syn_weather[i,21]=rgammatr(1,paramJan2$estimate["shape"],paramJan2$estimate["rate"],c(.02,.61))
      
    }
    else if (syn_weather[i,11]==2) {
      syn_weather[i,21]=rgammatr(1,paramFeb2$estimate["shape"],paramFeb2$estimate["rate"],c(.02,.61))
      
    }
    else if (syn_weather[i,11]==3) {
      syn_weather[i,21]=rgammatr(1,paramMar2$estimate["shape"],paramMar2$estimate["rate"],c(.02,.61))
      
    }
    else if (syn_weather[i,11]==4) {
      syn_weather[i,21]=rgammatr(1,paramApr2$estimate["shape"],paramApr2$estimate["rate"],c(.02,.61))
      
    }
    else if (syn_weather[i,11]==5) {
      syn_weather[i,21]=rgammatr(1,paramMay2$estimate["shape"],paramMay2$estimate["rate"],c(.02,.61))
  
    }
    else if (syn_weather[i,11]==6) {
      syn_weather[i,21]=rgammatr(1,paramJun2$estimate["shape"],paramJun2$estimate["rate"],c(.02,.61))

    }
    else if (syn_weather[i,11]==7) {
      syn_weather[i,21]=rgammatr(1,paramJul2$estimate["shape"],paramJul2$estimate["rate"],c(.02,.61))
    
    }
    else if (syn_weather[i,11]==8) {
      syn_weather[i,21]=rgammatr(1,paramAug2$estimate["shape"],paramAug2$estimate["rate"],c(.02,.61))
 
    }
    else if (syn_weather[i,11]==9) {
      syn_weather[i,21]=rgammatr(1,paramSep2$estimate["shape"],paramSep2$estimate["rate"],c(.02,.61))

    }
    else if (syn_weather[i,11]==10) {
      syn_weather[i,21]=rgammatr(1,paramOct2$estimate["shape"],paramOct2$estimate["rate"],c(.02,.61))
      
    }
    else if (syn_weather[i,11]==11) {
      syn_weather[i,21]=rgammatr(1,paramNov2$estimate["shape"],paramNov2$estimate["rate"],c(.02,.61))
      
    }
    else {
      syn_weather[i,21]=rgammatr(1,paramDec2$estimate["shape"],paramDec2$estimate["rate"],c(.02,.61))
      
    }
  } 
  
  else {
    syn_weather[i,21]=0
  }
  if (syn_weather[i,21]<0) {
    syn_weather[i,21]=0
  }
}


syn1=dplyr::filter(syn_weather,year<2010)
syn1=syn1[,c(2,12,13,20)]
hist1=dplyr::filter(hist_precip1,Year>=2006)
hist1=dplyr::filter(hist1,Year<2010)
hist1=hist1[1:nrow(syn1),]
hist1=hist1[,c(3,7:9,19,26)]
x=cbind(syn1,hist1)
x=na.omit(x)
ggplot(x)+
  geom_density(aes(syn_precip,fill="syn"))+
  geom_density(aes(PRCP,fill="act"),alpha=.5)+
  facet_wrap(~Month)

ggplot(x)+
  geom_histogram(aes(rain,fill="syn"))+
  geom_histogram(aes(simRainType,fill="act"),alpha=.5)+
  facet_wrap(~Month)

ggplot(x)+
  geom_histogram(aes(syn_precip,fill="syn"))+
  geom_histogram(aes(PRCP,fill="act"),alpha=.5)+
  facet_wrap(~Month)

mon=dplyr::group_by(x,Year,Month)%>%
  summarize(PRCP=sum(PRCP),syn_precip=sum(syn_precip))

ggplot(mon)+
  geom_histogram(aes(syn_precip,fill="syn"))+
  geom_histogram(aes(PRCP,fill="act"),alpha=.5)+
  facet_wrap(~Month)

x$mon=x$Month+.5
ggplot(x)+
  geom_density(aes(syn_precip,fill="syn"))+
  geom_density(aes(PRCP,fill="act"),alpha=.5)+
  facet_wrap(~Month)

ggplot(x)+
  geom_density(aes(rain,color="observed"))+
  geom_density(aes(simRainType,color="simulated"))+
  facet_wrap(~Month)+
  xlab("Month")+
  ylab("Number of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Wet Days, Monthly")

mean(syn_weather$syn_precip)
mean(hist_precip1$PRCP)

mean(syn_weather$simRainType)
mean(hist_precip1$rain)

write.csv(syn_weather,file="C:\\Users\\rcuppari\\Desktop\\syn_weather150yr_rain_ownTrend2.csv")
hist=hist_precip1[,c(16,5,6,4,2,10,18,26,3)]
syn=syn_weather[,c(2:8,13,20)]
corrplot(cor(hist,use="complete.obs"),method="color")
corrplot(cor(syn,use="complete.obs"),method="color")
