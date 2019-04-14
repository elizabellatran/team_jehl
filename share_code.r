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
library(ggbiplot)


#########
# set WD # 
###########
setwd('/Users/elizabella/Projects/BlueprintHack/')

#######
# Data #
####### 

STD = read_csv('stanford_blueprint_datathon_2019_data.csv')
var_interest= ('BRFSS Variables of Interest.xlsx')

########

# In order to run PCA, need to convert all strings into numeric
DF_exp<-STD[, 1:68] #subset all the exposure variables
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

pca_exposure = prcomp(na.omit(DF_exp), center = TRUE, scale. = TRUE)
summary(pca_exposure)

pca_rotation<-as.data.frame(pca_exposure$rotation)
write_csv(pca_rotation, "Exposures_PCA_rotations_v2.csv")

# Make some plots !
plot(pca_exposure, main = "variances of the principle components") # ranked graph of PC axes
ggsave("Variances_of_PCA.png")

ggbiplot(pca_exposure, obs.scale =1, var.scale=1,
            ellipse =FALSE, labels = NULL, var.axes = TRUE) + 
  theme_classic() +
  ggtitle("PCA of exposure variables") 
