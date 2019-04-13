######
#Libs #
######
library(knitr)
library(tidyverse)
library(reshape2)
library(dplyr)
library(ggplot2)

#########
# set WD # 
###########
setwd('/Users/elizabella/Projects/BlueprintHack/')

#######
# Data #
####### 

org_df = read_csv('stanford_blueprint_datathon_2019_data.csv')
var_interest= ('BRFSS Variables of Interest.xlsx')

########

