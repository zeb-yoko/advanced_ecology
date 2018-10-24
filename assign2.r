##Multivariate analysis workthrough##
##zebadiah.yoko@gmail.com##
##Assigment at end?##

##load libraries##
library(tidyverse)
library(vegan)

##load data##
data("dune")
data("dune.env")

##transform##
dt <- decostand(dune, method = "hellinger")

##Run Redundancy analysis##
rda1 <- rda(dt~., data = dune.env)
rda1







##run canonical correspondence analysis##
cca1a <- cca(dt~., data = varechem)
cca1a
