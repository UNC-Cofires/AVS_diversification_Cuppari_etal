##plots

rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)

ac_yield=read.table("C:\\Users\\rcuppari\\Desktop\\CleanAqua\\AquaCropOS_v50a\\Output\\Sample_FinalOutput_bar.txt",header=TRUE)
act_yield=read.csv("C:\\Users\\rcuppari\\Desktop\\CleanAqua\\AquaCropOS_v50a\\Output Salem\\Marion_Barley_NI.csv",header=TRUE)
act_yield=act_yield[,c(2,20)]
Date=as.Date(ac_yield$HarvestCD,format="%d/%m/%Y")
ac_yield=cbind(Date,ac_yield) 

ac_yield=mutate(ac_yield,
                   Year=substr(Date,1,4),
                   Month=substr(Date,6,7),
                   Day=substr(Date,9,10)) 

ac_yield$Year=as.numeric(ac_yield$Year)

compare=full_join(ac_yield,act_yield)
compare$Yield=compare$Yield*45.93/2.47 ##tons to bushels and ha to acres

ggplot(compare)+
  geom_point(aes(x=Value,y=Yield,color=factor(Year)))+
  xlab("Observed (bu/acre)")+
  ylab("Simulated (bu/acre)")+
  geom_abline()+
  annotate("text",x=40,y=30,label="r2=.24")+
  ggtitle("Barley Yields - AquaCrop v. Obs")

comp=dplyr::filter(compare,Year==1999 | Year==2000 | Year==2001)
rsq=function(x,y){cor(x,y)^2}
rsq(comp$Value,comp$Yield)

ggplot(compare)+
  geom_point(aes(x=Year,y=Value,color="actual"))+
  geom_point(aes(x=Year,y=Yield,color="observed"))+
  xlab("Year")+
  ylab("Yield (bu/acre)")+
  ggtitle("Yields")

trend=dplyr::group_by(compare,Year) %>% summarize(mean=mean(Value,na.rm=TRUE))
compare=full_join(compare,trend)
compare$det=0
compare$detSim=0
for (i in 2:nrow(compare)) {
  compare[i,15]=compare[i,13]-compare[(i-1),13]
  compare[i,16]=compare[i,8]-compare[(i-1),8]
}

ggplot(compare)+
  geom_point(aes(x=Year,y=det,color="detrended actual"))+
  geom_point(aes(x=Year,y=Yield,color="observed"))+
  xlab("Year")+
  ylab("Yield (bu/acre)")+
  ggtitle("Normalized Yields")
