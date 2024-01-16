##leap years
rm(list=ls())
library(tidyr) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     z
library(MASS)
library(RGeode)

syn_weather=read.csv("D:\\Research\\Synthetic_results\\Synthetic_weather\\syn_weather200yr_pstates.csv")

years=unique(syn_weather$year)
index=1:length(years)

for (i in index) {
  year=years[i]
  if (leap_year(year)==TRUE) {
    syn_weather[(nrow(syn_weather)+1),1]=0
    syn_weather[(nrow(syn_weather)),2]=0
    syn_weather[(nrow(syn_weather)),3]=0
    syn_weather[(nrow(syn_weather)),4]=0
    syn_weather[(nrow(syn_weather)),5]=0
    syn_weather[(nrow(syn_weather)),6]=0
    syn_weather[(nrow(syn_weather)),7]=0
    syn_weather[(nrow(syn_weather)),8]=0
    syn_weather[(nrow(syn_weather)),9]=year
    syn_weather[(nrow(syn_weather)),10]=29
    syn_weather[(nrow(syn_weather)),11]=2
    syn_weather[(nrow(syn_weather)),12]=0
    syn_weather[(nrow(syn_weather)),13]=0
  }
}

syn_weather=arrange(syn_weather,year,month,day)

for (i in 1:nrow(syn_weather)) {
  if (syn_weather[i,10]==29 & syn_weather[i,11]==2) {
    syn_weather[i,1]=syn_weather[(i-1),1]
    syn_weather[i,2]=syn_weather[(i-1),2]
    syn_weather[i,3]=syn_weather[(i-1),3]
    syn_weather[i,4]=syn_weather[(i-1),4]
    syn_weather[i,5]=syn_weather[(i-1),5]
    syn_weather[i,6]=syn_weather[(i-1),6]
    syn_weather[i,7]=syn_weather[(i-1),7]
    syn_weather[i,8]=syn_weather[(i-1),8]
    syn_weather[i,12]=syn_weather[(i-1),12]
    syn_weather[i,13]=syn_weather[(i-1),13]
    syn_weather[i,14]=syn_weather[(i-1),14]
    syn_weather[i,15]=syn_weather[(i-1),15]
  }  
} ##sigh - this took five minutes in R and I still haven't figured it out in python... :( 


syn_weather=syn_weather %>% group_by(year) %>%
  mutate(daynum2=seq_along(year))

syn_weather=syn_weather[,-1]
syn_weather[,3]=syn_weather[,16]
syn_weather=syn_weather[,-16]
write.csv(syn_weather,file="D:\\Research\\Synthetic_results\\Synthetic_weather\\syn_weather200yr_leap.csv")
syn_weather=read.csv("D:\\Research\\Synthetic_results\\Synthetic_weather\\syn_weather200yr_leap.csv")
