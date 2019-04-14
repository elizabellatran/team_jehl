########################
# Blueprint Hackathon:  Team jehl 
# Elizabeth Tran
# Jessica Hinman 
# Hoda S. Abdel Magid
# Lori Ling
#
# 4/13/19
###########################

######
#Libs #
######
library(knitr)
library(tidyverse)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggfortify)

#########
# set WD # 
###########
setwd('/Users/elizabella/Projects/BlueprintHack/')

#######
# Data #
####### 
org_df = read_csv('stanford_blueprint_datathon_2019_data.csv')
#var_interest= ('BRFSS Variables of Interest.xlsx')

########
# Theme #
theme<-theme(panel.background = element_blank(),panel.border=element_rect(fill=NA),
             panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
             strip.background=element_blank(),axis.text.x=element_text(colour="black"),
             axis.text.y=element_text(colour="black"),
             axis.ticks=element_line(colour="black"), 
             plot.margin=unit(c(1,1,1,1),"line"))
########
# PCA  # 
# 1-68 #
#######

df_PCA =  org_df[, 1:68]

#making all strings into numeric & as a new df 
for(i in colnames(df_PCA)){
  j<- as.factor(df_PCA[[i]]) %>% as.numeric()
  #reassign vector into df 
  df_PCA[[i]]= j
}

new_df_PCA=df_PCA

#PCA 
pca_exposure = prcomp(na.omit(new_df_PCA), scale. =TRUE , center= TRUE)

#screen plot
screeplot(pca_exposure)
plot(pca_exposure, main = "PCA Components in Predictor Space")
plot(pca_exposure, main = "PCA Components in Predictor Space", type = "l")

#PCA plots 
autoplot(pca_exposure, x=1, y=2) + ggtitle("PCA Exposure 1v2")
ggsave("pca_exposure_1v2.pdf")
autoplot(pca_exposure, x=1, y=3 ) + ggtitle("PCA Exposure 1v3")
ggsave("pca_exposure_1v3.pdf")
autoplot(pca_exposure, x=2, y=3 ) + ggtitle("PCA Exposure 2v3")
ggsave("pca_exposure_2v3.pdf")
autoplot(pca_exposure, x=2, y=4)+ + ggtitle("PCA Exposure 2v4")
ggsave("pca_exposure_2v4.pdf")


######summary of PCA variables##### 
summary(pca_exposure)

#getting eigenvalue
ev= pca_exposure$sdev^2
#write(ev, 'eigen_values.csv')

#looking at variables 
pca_rotation = pca_exposure$rotation
pca_rotation = as.data.frame(pca_rotation)


#################################################
###                  Trend Model
#################################################

library(tidyr)
library(dplyr)

df_PCA_screen = cbind(new_df_PCA, org_df[,69:79])
str(df_PCA_screen)

#plotting trends of screening over years -- grouping by age 
df_PCA_screen %>% group_by(age, gender, state, income, education)  %>% na.omit()%>% summarise(avg=mean(std_screen)) %>% 
  ggplot( aes(x = date, y = std_screen, group= interaction(age, state), color=age))+
  geom_point(size = 2) + ggtitle("STD Screening Over Time")

