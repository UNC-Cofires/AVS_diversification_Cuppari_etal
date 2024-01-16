##Salem precip
##prcp distrib
rm(list=ls())
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

Irrad=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Historical Data\\Salem\\DailyIrrad.csv")
Irrad=Irrad[,c(2:5,9)]

Irrad=group_by(Irrad,Year,Month,Day) %>%
  summarize(GHI=sum(GHI))
Irrad$Year=as.numeric(Irrad$Year)
Irrad$Month=as.numeric(Irrad$Month)
Irrad$Day=as.numeric(Irrad$Day)
Irrad=as.data.frame(Irrad)

WeatherData=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Historical Data\\Salem\\salem_weather2.csv") ##need for PRCP and SNOW##
Date=as.Date(WeatherData$DATE,format="%m/%d/%Y")
WeatherData=cbind(Date,WeatherData) 

WeatherData=mutate(WeatherData,
                   Year=substr(Date,1,4),
                   Month=substr(Date,6,7),
                   Day=substr(Date,9,10)) 

Sal=as.data.frame(WeatherData)
Sal$Year=as.numeric(Sal$Year)
Sal$Month=as.numeric(Sal$Month)
Sal$Day=as.numeric(Sal$Day)

SalWTI=full_join(Sal,Irrad)

SalWTI=SalWTI %>% group_by(Year) %>%
  mutate(daynum=seq_along(Year))

SalWTI=SalWTI[,c(5:14)]

SalWTI$rain=0

SalWTI=as.data.frame(SalWTI)

threshold=quantile(SalWTI$PRCP,0.95,na.rm=TRUE)

for (i in 1:nrow(SalWTI)) {
  if (is.na(SalWTI[i,2])==TRUE) {
    SalWTI[i,1]=0
  }
    else if (SalWTI[i,2]<0.02) {
    SalWTI[i,11]=0
  } else if (SalWTI[i,2]>.61) {
    SalWTI[i,11]=2} 
  else {
    SalWTI[i,11]=1
  }
}

for (i in 1:nrow(SalWTI)) {
  SalWTI[i,3]=(SalWTI[i,4]+SalWTI[i,5])/2
}

dayStats=dplyr::group_by(SalWTI,daynum) %>%
  summarize(mMax=mean(TMAX,na.rm=TRUE),sdMax=sd(TMAX,na.rm=TRUE),
            mMin=mean(TMIN,na.rm=TRUE),sdMin=sd(TMIN,na.rm=TRUE),
            mW=mean(AWND,na.rm=TRUE),sdW=sd(AWND,na.rm=TRUE),
            mI=mean(GHI,na.rm=TRUE),sdI=sd(GHI,na.rm=TRUE),
            mAvg=mean(TAVG,na.rm=TRUE),sdAvg=sd(TAVG,na.rm=TRUE))

SalDet=full_join(dayStats,SalWTI)

SalDet$lag=0
SalDet$lag2=0

SalDet[1,22]=0
SalDet[2,22]=0
SalDet[1,23]=0
SalDet[2,23]=SalDet[1,21]

for (i in 3:nrow(SalDet)) {
  SalDet[i,22]=SalDet[(i-1),21]
  SalDet[i,23]=SalDet[(i-2),21]
}

SalDet$TMAX.1=0
SalDet$TMIN.1=0
SalDet$AWND.1=0
SalDet$GHI.1=0
SalDet$TAVG.1=0

for (i in 1:nrow(SalDet)) {
  SalDet[i,24]=(SalDet[i,15]-SalDet[i,2])/SalDet[i,3] ##tmax
  SalDet[i,25]=(SalDet[i,16]-SalDet[i,4])/SalDet[i,5] ##tmin
  SalDet[i,26]=(SalDet[i,12]-SalDet[i,6])/SalDet[i,7] ##wind
  SalDet[i,27]=(SalDet[i,20]-SalDet[i,8])/SalDet[i,9] ##irrad
  SalDet[i,28]=(SalDet[i,14]-SalDet[i,10])/SalDet[i,11] ##tavg
}

ggplot(SalDet)+
  geom_density(aes(x=rain))+facet_wrap(~Month)

#write.csv(SalDet,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\SalDet0.01.csv")

SalDet$type=0 ##type = seasonal ish classification (wet/dry/in between)

for (i in 1:nrow(SalDet)) { 
  if (SalDet[i,18]==1 | SalDet[i,18]==12 | SalDet[i,18]==11 ) {
    SalDet[i,29]=5
  } else if (SalDet[i,18]==2 | SalDet[i,18]==3 | SalDet[i,18]==4 | SalDet[i,18]==10) {
    SalDet[i,29]=3
  } else if (SalDet[i,18]==9 | SalDet[i,18]==6 | SalDet[i,18]==5) {
    SalDet[i,29]=1
  } else {
      SalDet[i,29]=0    
  }
}

SalDet=arrange(SalDet,Year,Month,Day)

dry1=0
wet1=0
vwet1=0

dry2=0
wet2=0
vwet2=0

dry3=0
wet3=0
vwet3=0

dry4=0
wet4=0
vwet4=0

dry5=0
wet5=0
vwet5=0

dry6=0
wet6=0
vwet6=0

dry7=0
wet7=0
vwet7=0

dry8=0
wet8=0
vwet8=0

dry9=0
wet9=0
vwet9=0

dry10=0
wet10=0
vwet10=0

dry11=0
wet11=0
vwet11=0

dry12=0
wet12=0
vwet12=0

for (i in 1:nrow(SalDet)) {
  if (SalDet[i,17]>1997) {
    
    if (SalDet[i,21]==0) {
      if (SalDet[i,18]==1) {
        dry1=dry1+1
      } else if (SalDet[i,18]==2) {
        dry2=dry2+1
      } else if (SalDet[i,18]==3) {
        dry3=dry3+1
      } else if (SalDet[i,18]==4) {
        dry4=dry4+1 
      } else if (SalDet[i,18]==5) {
        dry5=dry5+1
      } else if (SalDet[i,18]==6) {
        dry6=dry6+1
      } else if (SalDet[i,18]==7) {
        dry7=dry7+1
      } else if (SalDet[i,18]==8) {
        dry8=dry8+1
      } else if (SalDet[i,18]==9) {
        dry9=dry9+1
      } else if (SalDet[i,18]==10) {
        dry10=dry10+1
      } else if (SalDet[i,18]==11) {
        dry11=dry11+1
      } else {
        dry12=dry12+1
      }
      
    } else if (SalDet[i,21]==1) {
      if (SalDet[i,18]==1) {
        wet1=wet1+1
      } else if (SalDet[i,18]==2) {
        wet2=wet2+1
      } else if (SalDet[i,18]==3) {
        wet3=wet3+1
      } else if (SalDet[i,18]==4) {
        wet4=wet4+1 
      } else if (SalDet[i,18]==5) {
        wet5=wet5+1
      } else if (SalDet[i,18]==6) {
        wet6=wet6+1
      } else if (SalDet[i,18]==7) {
        wet7=wet7+1
      } else if (SalDet[i,18]==8) {
        wet8=wet8+1
      } else if (SalDet[i,18]==9) {
        wet9=wet9+1
      } else if (SalDet[i,18]==10) {
        wet10=wet10+1
      } else if (SalDet[i,18]==11) {
        wet11=wet11+1
      } else {
        wet12=wet12+1
      }
      
      
    } else {
      if (SalDet[i,18]==1) {
        vwet1=vwet1+1
      } else if (SalDet[i,18]==2) {
        vwet2=vwet2+1
      } else if (SalDet[i,18]==3) {
        vwet3=vwet3+1
      } else if (SalDet[i,18]==4) {
        vwet4=vwet4+1 
      } else if (SalDet[i,18]==5) {
        vwet5=vwet5+1
      } else if (SalDet[i,18]==6) {
        vwet6=vwet6+1
      } else if (SalDet[i,18]==7) {
        vwet7=vwet7+1
      } else if (SalDet[i,18]==8) {
        vwet8=vwet8+1
      } else if (SalDet[i,18]==9) {
        vwet9=vwet9+1
      } else if (SalDet[i,18]==10) {
        vwet10=vwet10+1
      } else if (SalDet[i,18]==11) {
        vwet11=vwet11+1
      } else {
        vwet12=vwet12+1
      }
      
    }
    
  }
  }

SalDet$pwet=0
SalDet$pvwet=0

for (i in 1:nrow(SalDet)) {
  if (SalDet[i,18]==1) {

    SalDet[i,30]= (wet1+1)/(wet1+vwet1+2) #wet
    SalDet[i,31]= (vwet1+1)/(wet1+vwet1+2) #v wet
  } else if (SalDet[i,18]==2) {

    SalDet[i,30]= (wet2+1)/(wet2+vwet2+2) #wet
    SalDet[i,31]= (vwet2+1)/(wet2+vwet2+2) #v wet
  } else if (SalDet[i,18]==3) {

    SalDet[i,30]= (wet3+1)/(wet3+vwet3+2) #wet
    SalDet[i,31]= (vwet3+1)/(wet3+vwet3+2) #v wet
  } else if (SalDet[i,18]==4) {

    SalDet[i,30]= (wet4+1)/(wet4+vwet4+2) #wet
    SalDet[i,31]= (vwet4+1)/(wet4+vwet4+2) #v wet
  } else if (SalDet[i,18]==5) {

    SalDet[i,30]= (wet5+1)/(wet5+vwet5+2) #wet
    SalDet[i,31]= (vwet5+1)/(wet5+vwet5+2) #v wet
  } else if (SalDet[i,18]==6) {

    SalDet[i,30]=(wet6+1)/(wet6+vwet6+2) #wet
    SalDet[i,31]=(vwet6+1)/(wet6+vwet6+2) #v wet
  } else if (SalDet[i,18]==7) {

    SalDet[i,30]= (wet7+1)/(wet7+vwet7+2) #wet
    SalDet[i,31]= (vwet7+1)/(wet7+vwet7+2) #v wet
  } else if (SalDet[i,18]==8) {

    SalDet[i,30]= (wet8+1)/(wet8+vwet8+2) #wet
    SalDet[i,31]= (vwet8+1)/(wet8+vwet8+2) #v wet
  } else if (SalDet[i,18]==9) {

    SalDet[i,30]= (wet9+1)/(wet9+vwet9+2) #wet
    SalDet[i,31]= (vwet9+1)/(wet9+vwet9+2) #v wet
  } else if (SalDet[i,18]==10) {

    SalDet[i,30]= (wet10+1)/(wet10+vwet10+2) #wet
    SalDet[i,31]= (vwet10+1)/(wet10+vwet10+2) #v wet
  } else if (SalDet[i,18]==11) {

    SalDet[i,30]= (wet11+1)/(wet11+vwet11+2) #wet
    SalDet[i,31]= (vwet11+1)/(wet11+vwet11+2) #v wet
  } else {

    SalDet[i,30]= (wet12+1)/(wet12+vwet12+2) #wet
    SalDet[i,31]= (vwet12+1)/(wet12+vwet12+2) #v wet
  }
}

ggplot(SalDet)+geom_point(aes(x=Month,y=pvwet))
ggplot(SalDet)+geom_point(aes(x=Month,y=pwet))

SalDet$rain1=0

for (i in 1:nrow(SalDet)) {
  if (SalDet[i,21]==0) {
    SalDet[i,32]=0
  } else {
    SalDet[i,32]=1
  }
}

write.csv(SalDet,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\SalDet2s1997.csv")


#SalDet=read.csv("C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\SalDet2s19972.csv")
##detrended precipitation annually for SalDet
######so what if we try to simulate number of wet/dry days per MONTH? 
SalDet$dry=0
SalDet$wet=0
SalDet$vwet=0

for (i in 1:nrow(SalDet)) {
  if (SalDet[i,20]==0) {
    SalDet[i,29]=1
    SalDet[i,30]=0
    SalDet[i,31]=0
  } else if (SalDet[i,20]==1) {
    SalDet[i,29]=0
    SalDet[i,30]=1
    SalDet[i,31]=0
  } else {
    SalDet[i,29]=0
    SalDet[i,30]=0
    SalDet[i,31]=2
  }
}

stats=dplyr::group_by(SalDet,Year,Month)%>%
  summarize(dryMon=sum(dry),wetMon=sum(wet),vwetMon=sum(vwet),
            tmax=max(TMAX,na.rm=TRUE),mmax=mean(TMAX,na.rm=TRUE),
            tmin=min(TMIN,na.rm=TRUE), mmin=mean(TMIN,na.rm=TRUE),
            mGHI=mean(GHI,na.rm=TRUE),minI=min(GHI,na.rm=TRUE),
            maxI=max(GHI,na.rm=TRUE))

SalSep=full_join(SalDet,stats)

#write.csv(stats,file="C:\\Users\\rcuppari\\OneDrive\\Research\\Syn_generator_include_irr-master\\Historical_weather_analysis\\SalSep.csv")
