# -*- coding: utf-8 -*-
"""
Created on Mon Nov 18 11:02:59 2019

@author: rcuppari
"""

normYield=read.delim("C:\\Users\\rcuppari\\Desktop\\CleanAqua\\AquaCropOS_v50a\\Output\\Bar_Salem_Syn150_1906_FinalOutput.txt")
AVSyield=read.delim("C:\\Users\\rcuppari\\Desktop\\CleanAqua\\AquaCropOS_v50a\\Output\\Bar_Salem_Syn150_1906_AVS_FinalOutput.txt")
normYield=normYield[,c(1,7)]
AVSyield=AVSyield[,c(1,7)]
bar93prices=read.csv("C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Synthetic_results\\ag\\barleyPrices150.csv")
bar93prices=as.data.frame(bar93prices[,2])
powgen2=read.csv("C:\\Users\\rcuppari\\OneDrive - University of North Carolina at Chapel Hill\\Research\\Synthetic_results\\stochgen_solrev150.csv")

def revenues(powgen,ppa_low,ppa_high,ppa_blend):   
landAVS=20
landtot=111
landNon=landtot-landAVS
solCap=4.6 ##installation capacity, MWdc
coverage=.7
sol_reduction=.8
taxCred26=.26
taxCred10=.10
valueInstall=4532000 
landAVS_percent=coverage*sol_reduction ##percent covered by panels in the AVS system - .8 of the .7 originally covering
barReduc=.9 ##how much less barley producing in AVS - diminish cost by that flat amount

    
    
    return None 

##revenue calculation 
powgen2$rev=powgen2$GenGHI*powgen2$price ##total revenue of non-AVS solar farm 
powgen2$AVSrev=powgen2$AVSgen*powgen2$price ##total revenue of AVS farm 

powgen2$revM=powgen2$UnitGen*powgen2$price ##per acre revenue of non-AVS farm
powgen2$AVSrevM=powgen2$unitAVSgen*powgen2$price##per acre revenue of AVS farm



def net_income():
    
    return None