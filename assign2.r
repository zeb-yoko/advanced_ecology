##Multivariate analysis assignment##
##zebadiah.yoko@gmail.com##

##load libraries##
library(tidyverse)
library(vegan)

##load data##
data("dune")
data("dune.env")

##transform##
dt <- decostand(dune, method = "hellinger")

##Run Redundancy analysis##
##using formula format##
rda1 <- rda(dt~., data = dune.env)
rda1
##1-- variance explained = .7410##
##2-- explained by axis 1 = .15375##

##Permutation analysis##
set.seed(1)
perm<-anova(rda1, by= "axis")
##RDA1 appears to be only significant one##

##4.	Conduct a partial constrained RDA on the transformed 
#species data to examine the importance of Soil type (A1) 
#when the influence of Moisture is Removed##
##
dt
str(dt)
str(dune.env)
##can't find Condition function##
??Condition
##partial conditioned rda formula##
##all env var -Moisture##
rda.p<-rda(dt~A1+Management+Use+Manure, data = dune.env)
rda.p
##4--.5811 percent 
