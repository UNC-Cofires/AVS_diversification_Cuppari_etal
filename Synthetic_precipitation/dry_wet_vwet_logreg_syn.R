##prcp plots for eugene
rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(Metrics)

actual=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\actualSal19971syn.csv")
predict=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\predictedStateSal19971syn.csv")
rainStateVal=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\simRainSalSyn1.csv")

colnames(actual)=c("day","val")
colnames(predict)=c("day","predict")
compare=full_join(actual,predict)

rainStateVal$dry=0
rainStateVal$wet=0
rainStateVal$vwet=0

rainStateVal$adry=0
rainStateVal$awet=0
rainStateVal$avwet=0

for (i in 1:nrow(rainStateVal)) {
  if (is.na(rainStateVal[i,40])==TRUE) {
    rainStateVal[i,40]=0
  }
  if (rainStateVal[i,40]==0) {
    rainStateVal[i,41]=1 ##sim dry day 
    rainStateVal[i,42]=0
    rainStateVal[i,43]=0
    
  } else if (rainStateVal[i,40]==1) {
    rainStateVal[i,41]=0
    rainStateVal[i,42]=1
    rainStateVal[i,43]=0
    
  } else {
    rainStateVal[i,41]=0
    rainStateVal[i,42]=0
    rainStateVal[i,43]=1
  }
  
  if (rainStateVal[i,25]==0) {
    rainStateVal[i,44]=1 ##actual dry day 
    rainStateVal[i,45]=0
    rainStateVal[i,46]=0
    
  } else if (rainStateVal[i,25]==1) {
    rainStateVal[i,44]=0
    rainStateVal[i,45]=1
    rainStateVal[i,46]=0
    
  } else {
    rainStateVal[i,44]=0
    rainStateVal[i,45]=0
    rainStateVal[i,46]=1
  }
  
}

totAW=sum(rainStateVal$awet)
totW=sum(rainStateVal$wet)
diffWet=(totAW-totW)/totAW

totAD=sum(rainStateVal$adry)
totD=sum(rainStateVal$dry)
diffDry=(totAD-totD)/totAD

totAVW=sum(rainStateVal$avwet)
totVW=sum(rainStateVal$vwet)
diffVWet=(totAVW-totVW)/totAVW

corvars=rainStateVal[,c(15:19,23,24,37)]
library(corrplot)
corrs=cor(corvars,use="complete.obs")
corrplot(corrs,method="color")

mR=dplyr::group_by(rainStateVal,Year,Month)%>%
  summarize(adry=sum(adry),dry=sum(dry),
            awet=sum(awet),wet=sum(wet),
            avwet=sum(avwet),vwet=sum(vwet),
            rain=sum(rain),simRain=sum(simRainType))

rsq=function(x,y) {cor(x,y)^2}
rsq(mR$avwet,mR$vwet)
rsq(mR$awet,mR$wet)
rsq(mR$adry,mR$dry)
rsq(mR$rain,mR$simRain)
rsq((mR$avwet+mR$awet),(mR$vwet+mR$wet))

mR$diffO=0
mR$diffM=0
for (i in 1:nrow(mR)) {
  mR[i,12]=(mR[i,9]-mR[i,10])^2
  mR[i,13]=(mR[i,9]-mean(mR$rain))^2
}

sumO=sum(mR[,12])
sumM=sum(mR[,13])
1-sumO/sumM

mR$month2=mR$Month+.5

sum(rainStateVal$wet)
sum(rainStateVal$awet)

sum(rainStateVal$vwet)
sum(rainStateVal$avwet)

sum(rainStateVal$dry)
sum(rainStateVal$adry)


##########wet dry spells###########
compare=full_join(actual,predict)

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
  if (compare[i,2]>0) {
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
  if (compare[i,2]>0) {
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

(avgLenWetTest-avgLenWetAct)/avgLenWetAct
(avgLenDryTest-avgLenDryAct)/avgLenDryAct

##################plots######################
ggplot(compare)+
  geom_histogram(aes(x=val,fill="observed"),alpha=1)+
  geom_histogram(aes(x=predict,fill="simulated"),alpha=.5)+
  annotate("text",x=1,y=4000,label="dry=0, wet=1, very wet =2")+
  ggtitle("Histogram of observed versus simulated rain states")


ggplot(mR)+
  geom_boxplot(aes(x=month2,y=rain,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=simRain,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Number of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Summed Rainy Days, Monthly")+
  annotate("text",x=7,y=28,label="r2=0.84")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("purple","orange"))#+

ggplot(mR)+
  geom_boxplot(aes(x=month2,y=(awet+avwet),group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=(wet+vwet),group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Number of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Rainy Days, Monthly")+
  annotate("text",x=7,y=28,label="r2=0.83")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("purple","orange"))#+


ggplot(mR)+
  geom_boxplot(aes(x=month2,y=adry,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=dry,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Number of Dry Days")+
  ggtitle("Simulated v Observed Amount of Dry Days, Monthly")+
  annotate("text",x=7,y=5,label="r2=0.83")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("purple","orange"))#+

ggplot(mR)+
  geom_boxplot(aes(x=month2,y=awet,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=wet,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Number of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Wet Days, Monthly")+
  #annotate("text",x=7,y=28,label="r2=0.91")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("purple","orange"))#+


ggplot(mR)+
  geom_boxplot(aes(x=month2,y=avwet,group=Month,color="observed"),width=.4,position=position_dodge())+
  geom_boxplot(aes(x=Month,y=vwet,group=Month,color="simulated"),width=.4,position=position_dodge())+
  xlab("Month")+
  ylab("Number of Rainy Days")+
  ggtitle("Simulated v Observed Amount of Very Wet Days, Monthly")+
  #annotate("text",x=7,y=28,label="r2=0.91")+
  scale_x_continuous(breaks = c(1.25,2.25,3.25,4.25,5.25,6.25,
                                7.25,8.25,9.25,10.25,11.25,12.25),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_color_manual("Data",values=c("purple","orange"))#+

ggplot(mR)+
  geom_point(aes(x=awet,y=wet,color=factor(Month)))+
  facet_wrap(~Month)+geom_abline()+
  ggtitle("Observed v Simulated Wet Days")

ggplot(mR)+
  geom_point(aes(x=adry,y=dry,color=factor(Month)))+
  facet_wrap(~Month)+geom_abline()+
  ggtitle("Observed v Simulated Dry Days")

ggplot(mR)+
  geom_point(aes(x=avwet,y=vwet,color=factor(Month)))+
  facet_wrap(~Month)+geom_abline()+
  ggtitle("Observed v Simulated Very Wet Days")

ggplot(rainStateVal)+
  geom_histogram(aes(rain,fill="observed"),bins=5)+
  geom_histogram(aes(simRainType,fill="simulated"),alpha=.4,bins=5)+
  facet_wrap(~Month)+
  ggtitle("Histogram of Rain States")

ggplot(rainStateVal)+
  geom_density(aes(awet),color="purple",fill="purple",bins=5)+
  geom_density(aes(wet),color="orange",fill="orange",alpha=.4,bins=5)+
  facet_wrap(~Month)+
  ggtitle("Histogram of Monthly Wet Days")

ggplot(rainStateVal)+
  geom_density(aes(adry),color="purple",fill="purple",bins=5)+
  geom_density(aes(dry),color="orange",fill="orange",alpha=.4,bins=5)+
  facet_wrap(~Month)+
  ggtitle("Histogram of Monthly Dry Days")

ggplot(rainStateVal)+
  geom_density(aes(avwet,fill="observed"),bins=5)+
  geom_density(aes(vwet,fill="simulated"),alpha=.4,bins=5)+
  facet_wrap(~Month)+
  ggtitle("Histogram of Monthly Very Wet Days")

ggplot(rainStateVal)+
  geom_density(aes(avwet+awet,fill="observed"),bins=5)+
  geom_density(aes(vwet+wet,fill="simulated"),alpha=.4,bins=5)+
  facet_wrap(~Month)+
  ggtitle("Histogram of Monthly Rainy Days")

