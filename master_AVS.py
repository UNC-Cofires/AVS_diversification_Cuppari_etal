# -*- coding: utf-8 -*-
"""
Created on Mon Nov 18 11:23:20 2019

@author: rcuppari
"""

#######################################
#######################################
##master script to calculate cashflow
##for solar, farm, and AVS
#######################################
#######################################

import synthetic_max_min(weather) 
print('synMaxMin')

import synthetic_precip(weather2) 
print('synP')

import synthetic_power_OSU(acres,weather)
print('powgen')

##NEED TO IMPORT MATLAB FOR AQUACROP 

import cashflow(powgen,aquacrop)
print('net_income')