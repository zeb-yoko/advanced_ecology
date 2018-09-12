########ADVANCED ECOLOGY VERSION#########
##Zebadiah.Yoko@gmail.com##

##load important libraries##
library(car); library(dhglm); library(effects);
library(glmm); library(gridExtra); library(lme4)
library(lmerTest);library(mcmc); library(plotly); library(reshape2); 
library(rptR); library(corrr); library(tidyverse)
#setwd("C:/Users/zebadiah/NDSU Dropbox/Dropbox/Geum triflorum/Master_Datasheets")
##ASUS
setwd("C:/Users/zippy/Dropbox/Geum triflorum/Master_Datasheets")
#set colors   #Prairie   #GL ALvar  #MB Alvar
col.esa <- c("#18563E", "#82BE42", "#FFC423")
setwd("C:/Users/zippy/OneDrive/NDSU/Rstudio-code")
#csv loadin workaround
colnames(VECTOR)[1] = "Column_Name"

