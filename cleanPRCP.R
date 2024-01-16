##prcp distrib
rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)


##EUGENE PRCP DATA
eugData=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\eugHistWeather_Clean.csv") 
eugP=eugData[,c(3,7:9)]

for (i in 1:nrow(eugP)) {
  if (eugP[i,1]==0) {
    eugP[i,5]=0
  } else if (eugP[i,1]>0 & eugP[i,1]<=1) {
    eugP[i,5]=1
  } else {
    eugP[i,5]=2
  }
}

daynumP=eugP %>% group_by(Year) %>%
  mutate(daynum=seq_along(Year))

##let's get stats of wet ones only, by intensity
pDayStat=group_by(daynumP,daynum,V5)%>%
  summarize(mP=mean(PRCP,na.rm=TRUE),sdP=sd(PRCP,na.rm=TRUE))

eugP=full_join(daynumP,pDayStat)

eugWet=dplyr::filter(eugP,V5>0)
ggplot(eugWet)+
  geom_density(aes(log(PRCP)))+
  facet_wrap(~Month)
  
##detrend data based on category 
eugP=as.data.frame(eugP)
for (i in 1:nrow(eugP)) {
  if (eugP[i,1]>0) {
    eugP[i,9]=(eugP[i,1]-eugP[i,7])
  } else {
    eugP[i,9]=0
  }
} ## daily  stats

colnames(eugP)=c("PRCP","Year","Month","Day","rain","daynum",
                 "mP","sdP","detP") ##detrended by mean of intensity 

ggplot(eugP)+
  geom_histogram(aes(x=detP),bins=30)+
  facet_wrap(~Month)

#write.csv(eugP,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\eugP_meanDet.csv")

eugP_med=dplyr::filter(eugP,rain==1)
eugP_high=dplyr::filter(eugP,rain==2)

ggplot(eugP_med)+
  geom_density(aes((log(detP+1.301))))+
  facet_wrap(~Month)

ggplot(eugP_high)+
  geom_density(aes(PRCP))+
  facet_wrap(~Month)
















ggplot(eugP)+
  geom_density(aes(log(V9+.4)))+
  facet_wrap(~Month)

ggplot(eugP_med)+
  geom_histogram(aes(x=log(V9+.1)),bins=100)+
  facet_wrap(~Month)

ggplot(eugP_high)+
  geom_histogram(aes(x=log(V9+1)),bins=100)+
  facet_wrap(~Month)

#write.csv(eugP_med,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\eugP_med.csv")
#write.csv(eugP_high,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\eugP_high.csv")
#write.csv(eugP2,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\eugP_Wet.csv")
#write.csv(eugP,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\eugP_1mm.csv")
