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
library(xlsx)
library(lme4)
library(AICcmodavg)


########
# Data #
####### 
org_df = read.csv('stanford_blueprint_datathon_2019_data.csv')
#var_interest= ('BRFSS Variables of Interest.xlsx')

#########
# Theme #
########
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

DF_exp =  org_df[, 1:68]

# In order to run PCA, need to convert all strings into numeric
colnames(DF_exp)
str(DF_exp)
for (i in colnames(DF_exp)) {
  print(i)
  if (is.character(DF_exp[[i]]) == TRUE) {
    j <- as.factor(DF_exp[[i]]) %>% as.numeric()
    #print(j)
    DF_exp[[i]] <- j 
  }
}

new_df_PCA=df_exp

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
#Use PC1 - PC12 in analyses due to eigenvalues > 1.

#looking at variables 
pca_rotation = pca_exposure$rotation
pca_rotation = as.data.frame(pca_rotation)

###########################################################
#Merge PCs 1-12 into original dataset to use in analyses
STI_long <- cbind(org_df, pca_exposure$x[,1:12])

#Create ID variable for transformation and analyses. Takes a long time to run, did not optimize. Run at your own risk.
#for (row in 1:nrow(STI_long)) {
#  #Get factors in numeric format to create unique ID per combination
#  age = paste("0",as.character(as.numeric(STI_long$age[row])), sep="")
#  gender = paste("0",as.character(as.numeric(STI_long$gender[row])), sep="")
#  state = as.character(as.numeric(STI_long$state[row]))
#  income = as.character(as.numeric(STI_long$income[row]))
#  education = paste("0",as.character(as.numeric(STI_long$education[row])), sep="")
#  
#  #Check length where needed, append 0 if factor level is 0-9
#  if(nchar(state) < 2) {state = paste("0",state,sep="")}
#  if(nchar(income) < 2) {income = paste("0",income,sep="")}
#  
#  #Create ID
#  ID = paste(age,gender,state,income,education,sep="")
#  if(row %% 10000 == 0) {print(paste(row,"- ID is:",ID))}
#  STI_long$ID[row] = ID
#}

#Save IDs to save time on future runs
write.csv(STI_long$ID, file = "IDs.csv")

#Transform data from long form to wide form for longitudinal analyses
#STI <- reshape(STI_long, idvar = 'ID', timevar = 'date', direction = 'wide', drop = c())

#Create summary outcome variable representing average diagnosis count ratio
STI$mean_count <- rowMeans(subset(STI, select = c(chlamydia, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich)), na.rm = TRUE)
STI$mean_count[is.nan(STI$mean_count)] <-NA


#Subset by age groups to examine trends
AGE_1 <- subset(STI, age == '18-24 years old')
AGE_2 <- subset(STI, age == '25-34 years old')
AGE_3 <- subset(STI, age == '35-44 years old')
AGE_4 <- subset(STI, age == '45-54 years old')
AGE_5 <- subset(STI, age == '55-64 years old')
AGE_6 <- subset(STI, age == '65-74 years old')
AGE_7 <- subset(STI, age == '75+ years old')

#Create spaghetti plots of STI diagnosis trends over time by age group
interaction.plot(AGE_1$date, AGE_1$ID, AGE_1$mean_count, xlab = 'Time', ylab = 'Average STI Diagnosis Ratio', legend = F)
title(main = 'STI Diagnosis Rates in 18-24 Age Group')

interaction.plot(AGE_2$date, AGE_2$ID, AGE_2$mean_count, xlab = 'Time', ylab = 'Average STI Diagnosis Ratio', legend = F)
title(main = 'STI Diagnosis Rates in 25-34 Age Group')

interaction.plot(AGE_3$date, AGE_3$ID, AGE_3$mean_count, xlab = 'Time', ylab = 'Average STI Diagnosis Ratio', legend = F)
title(main = 'STI Diagnosis Rates in 35-44 Age Group')

interaction.plot(AGE_4$date, AGE_4$ID, AGE_4$mean_count, xlab = 'Time', ylab = 'Average STI Diagnosis Ratio', legend = F)
title(main = 'STI Diagnosis Rates in 45-54 Age Group')

interaction.plot(AGE_5$date, AGE_5$ID, AGE_5$mean_count, xlab = 'Time', ylab = 'Average STI Diagnosis Ratio', legend = F)
title(main = 'STI Diagnosis Rates in 55-64 Age Group')

interaction.plot(AGE_6$date, AGE_6$ID, AGE_6$mean_count, xlab = 'Time', ylab = 'Average STI Diagnosis Ratio', legend = F)
title(main = 'STI Diagnosis Rates in 65-74 Age Group')

interaction.plot(AGE_7$date, AGE_7$ID, AGE_7$mean_count, xlab = 'Time', ylab = 'Average STI Diagnosis Ratio', legend = F)
title(main = 'STI Diagnosis Rates in 75+ Age Group')

#Fit linear mixed effects models to summary outcome variable
#First evaluate random effects under REML with all potential fixed effects
#STI.lmer1 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + (date | ID), data = STI, REML = TRUE)
#Random slope and intercept model fails to converge without significant alterations to variable space and tolerance

STI.lmer2 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + (1 | ID), data = STI, REML = TRUE)

#Compare random effects models using AIC
RE.models <- list(STI.lmer1, STI.lmer2)
mod_names <- c('Random Slope and Intercept', 'Random Intercept Only')
print(aictab(RE.models, mod_names, second.ord = FALSE), LL = FALSE)

#Using Random Slope and Intercept, perform model selection on fixed effects using ML
STI.lmer3 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + (1 | ID), data = STI, REML = FALSE)

STI.lmer4 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + (1 | ID), data = STI, REML = FALSE)

STI.lmer5 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + (1 | ID), data = STI, REML = FALSE)

STI.lmer6 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + (1 | ID), data = STI, REML = FALSE)

STI.lmer7 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + (1 | ID), data = STI, REML = FALSE)

STI.lmer8 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + (1 | ID), data = STI, REML = FALSE)

STI.lmer9 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + (1 | ID), data = STI, REML = FALSE)

STI.lmer10 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + PC5 + (1 | ID), data = STI, REML = FALSE)

STI.lmer11 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + PC4 + (1 | ID), data = STI, REML = FALSE)

STI.lmer12 <- lmer(mean_count ~ date + PC1 + PC2 + PC3 + (1 | ID), data = STI, REML = FALSE)

STI.lmer13 <- lmer(mean_count ~ date + PC1 + PC2 + (1 | ID), data = STI, REML = FALSE)

STI.lmer14 <- lmer(mean_count ~ date + PC1 + (1 | ID), data = STI, REML = FALSE)

STI.lmer15 <- lmer(mean_count ~ date + (1 | ID), data = STI, REML = FALSE)

FE.models <- list(STI.lmer3, STI.lmer4, STI.lmer5, STI.lmer6, STI.lmer7, STI.lmer8, STI.lmer9, STI.lmer10, STI.lmer11, STI.lmer12, 
                  STI.lmer13, STI.lmer14, STI.lmer15)
mod_names2 <- c('Full', '11 PCs', '10 PCs', '9 PCs', '8 PCs', '7 PCs', '6 PCs', '5 PCs', '4 PCs', '3 PCs', '2 PCs', '1 PC', 'Time only')
print(aictab(FE.models, mod_names2, second.ord = FALSE), LL = FALSE)

summary(STI.lmer3)
#Check residuals on final model
plot(STI.lmer5, type = c('p','smooth'))
hist(residuals(STI.lmer3), 10)